/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.constants;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.wikipediacleaner.api.configuration.WikiConfiguration;
import org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings;
import org.wikipediacleaner.api.http.HttpUtils;


/**
 * Analysis of URL representing articles.
 */
public class ArticleUrl {

  private static Charset utf8Charset = null;
  private static Charset iso88591Charset = null;

  static {
    utf8Charset = Charset.forName("UTF8");
    iso88591Charset = Charset.forName("ISO-8859-1");
  }

  /**
   * @param wiki Wiki.
   * @param url URL.
   * @return Information about article if URL is a link to the given wiki.
   */
  public static ArticleUrl isArticleUrl(EnumWikipedia wiki, String url) {
    if ((wiki == null) || (url == null)) {
      return null;
    }
    ArticleUrl result = null;

    // Analysis based on Wiki settings
    AbstractWikiSettings settings = wiki.getSettings();
    if (settings != null) {
      for (String directPath : settings.getArticleDirectPath()) {
        if (result == null) {
          result = isArticleDirectUrl(url, directPath);
        }
      }
      for (String paramPath : settings.getArticleParamPath()) {
        if (result == null) {
          result = isArticleParamUrl(url, paramPath, "title");
        }
      }
    }

    // Analysis based on Wiki configuration
    WikiConfiguration wikiConf = wiki.getWikiConfiguration();
    if (wikiConf != null) {
      String server = wikiConf.getServer();
      String articlePath = wikiConf.getArticlePath();
      if ((result == null) && (server != null) && (articlePath != null)) {
        result = isArticleDirectUrl(url, server + articlePath);
      }
      String script = wikiConf.getScript();
      if ((result == null) && (server != null) && (script != null)) {
        result = isArticleParamUrl(url, server + script, "title");
      }
    }

    return result;
  }

  /**
   * @param url URL.
   * @return Corresponding URI.
   */
  private static URI getURI(String url) {
    if (url == null) {
      return null;
    }

    // Cleanup URL
    while (url.endsWith("|")) {
      url = url.substring(0, url.length() - 1);
    }

    // Create URI
    URI uri = null;
    try {
      uri = new URI(url);
    } catch (URISyntaxException e) {
      return null;
    }

    // Various checks
    if (!uri.isAbsolute() || uri.isOpaque()) {
      return null;
    }

    // Check scheme
    String scheme = uri.getScheme();
    if (scheme == null) {
      return null;
    }
    if (!scheme.equalsIgnoreCase("http") &&
        !scheme.equalsIgnoreCase("https")) {
      return null;
    }

    return uri;
  }

  /**
   * @param url URL.
   * @param base Base URL for accessing articles.
   * @return Information about article if URL is a link following the base URL.
   */
  private static ArticleUrl isArticleDirectUrl(String url, String base) {
    if ((url == null) || (base == null)) {
      return null;
    }

    // Cleanup URL
    while (url.endsWith("|")) {
      url = url.substring(0, url.length() - 1);
    }
    int colonIndex = url.indexOf(':');
    if (colonIndex >= 0) {
      int endSlash = colonIndex + 1;
      while ((endSlash < url.length()) && (url.charAt(endSlash) == '/')) {
        endSlash++;
      }
      if (endSlash > colonIndex + 3) {
        url = url.substring(0, colonIndex + 3) + url.substring(endSlash);
      }
    }

    // Create URI
    URI uri = getURI(url);
    if (uri == null) {
      return null;
    }

    // Check that URL is coherent with provided base
    StringBuilder buffer = new StringBuilder();
    buffer.append("//");
    buffer.append(uri.getAuthority());
    buffer.append(HttpUtils.parseEncodedString(uri.getPath(), utf8Charset, iso88591Charset));
    int paramIndex = base.indexOf("$1");
    if (paramIndex < 0) {
      return null;
    }
    String bufferStr = buffer.toString();
    if (bufferStr.length() <= paramIndex) {
      return null;
    }
    if (!bufferStr.startsWith(base.substring(0, paramIndex))) {
      return null;
    }

    // Retrieve optional attributes
    Map<String, String> paramValues = null;
    if (uri.getQuery() != null) {
      String[] params = uri.getQuery().split("\\&");
      if ((params != null) && (params.length > 0)) {
        paramValues = new HashMap<String, String>();
        for (String param : params) {
          String[] value = param.split("\\=", 2);
          if (value.length >= 1) {
            if (value.length >= 2) {
              paramValues.put(value[0], value[1]);
            } else {
              paramValues.put(value[0], null);
            }
          }
        }
      }
    }

    return new ArticleUrl(bufferStr.substring(paramIndex), paramValues, uri.getFragment());
  }

  /**
   * @param url URL.
   * @param base Base URL for accessing articles.
   * @param paramName Parameter name for title.
   * @return Information about article if URL is a link following the base URL.
   */
  private static ArticleUrl isArticleParamUrl(String url, String base, String paramName) {
    if ((url == null) || (base == null) || (paramName == null)) {
      return null;
    }

    // Cleanup URL
    while (url.endsWith("|")) {
      url = url.substring(0, url.length() - 1);
    }

    // Create URI
    URI uri = getURI(url);
    if (uri == null) {
      return null;
    }

    // Check that URL is coherent with provided base
    StringBuilder buffer = new StringBuilder();
    buffer.append("//");
    buffer.append(uri.getAuthority());
    buffer.append(HttpUtils.parseEncodedString(uri.getRawPath(), utf8Charset, iso88591Charset));
    String bufferStr = buffer.toString();
    if (!bufferStr.equals(base)) {
      return null;
    }

    // Check parameters
    if (uri.getQuery() == null) {
      return null;
    }
    String[] params = uri.getQuery().split("\\&");
    if ((params == null) || (params.length == 0)) {
      return null;
    }
    String title = null;
    Map<String, String> paramValues = new HashMap<String, String>();
    for (String param : params) {
      String[] value = param.split("\\=", 2);
      if (value.length >= 1) {
        if (paramName.equals(value[0])) {
          if (value.length >= 2) {
            title = value[1];
          }
        } else {
          if (value.length >= 2) {
            paramValues.put(value[0], value[1]);
          } else {
            paramValues.put(value[0], null);
          }
        }
      }
    }
    if (title == null) {
      return null;
    }

    return new ArticleUrl(title, paramValues, uri.getFragment());
  }

  /** Article title */
  private final String title;

  /** Attributes */
  private final Map<String, String> attributes;

  /** Fragment */
  private final String fragment;

  /**
   * @param title Article title.
   * @param attributes Attributes.
   */
  private ArticleUrl(String title, Map<String, String> attributes, String fragment) {
    String tmpTitle = title;
    if (tmpTitle != null) {
      tmpTitle = tmpTitle.replaceAll("\\_", " ");
      tmpTitle = tmpTitle.replaceAll("  ", " ");
      if (tmpTitle.endsWith("/")) {
        tmpTitle = tmpTitle.substring(0, tmpTitle.length() - 1);
      }
    }
    this.title = tmpTitle;
    this.attributes = (attributes != null) ? Collections.unmodifiableMap(attributes) : null;
    this.fragment = fragment;
  }

  /**
   * @return Article title.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @return Attributes.
   */
  public Map<String, String> getAttributes() {
    return attributes;
  }

  /**
   * @return Fragment (anchor).
   */
  public String getFragment() {
    return fragment;
  }

  /**
   * @return Title with optional fragment.
   */
  public String getTitleAndFragment() {
    if (fragment == null) {
      return title;
    }
    return title + "#" + fragment;
  }
}
