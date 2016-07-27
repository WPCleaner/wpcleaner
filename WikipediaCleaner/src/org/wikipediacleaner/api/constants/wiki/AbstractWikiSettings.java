/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;


/**
 * Abstract base class for wiki configuration.
 */
public abstract class AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   * @param hosts List of possible host names.
   * @param apiPath Path for the API, relative to the host URL.
   * @param indexPath Path for index.php, relative to the host URL.
   * @param code Code for the wiki.
   * @param codeCheckWiki CW code for the wiki.
   * @param orientation Text orientation.
   */
  public AbstractWikiSettings(
      String language, String name,
      String[] hosts, String apiPath, String indexPath,
      String code, String codeCheckWiki,
      ComponentOrientation orientation) {
    this.language = language;
    this.name = name;
    this.hosts = hosts;
    this.apiPath = apiPath;
    String host = hosts[0];
    this.apiUrl = "http://" + host + apiPath;
    this.securedApiUrl = "https://" + host + apiPath;
    this.indexPath = indexPath;
    this.indexUrl = "http://" + host + indexPath;
    this.securedIndexUrl = "https://" + host + indexPath;
    this.code = code;
    this.codeCheckWiki = codeCheckWiki;
    this.orientation = orientation;
  }

  /** Language */
  private final String language;

  /**
   * @return Language.
   */
  public final String getLanguage() {
    return language;
  }

  /** Name */
  private final String name;

  /**
   * @return Name.
   */
  public final String getName() {
    return name;
  }

  /**
   * @return True if connection can be secured.
   */
  protected boolean canBeSecured() {
    return true;
  }

  /** Host names */
  private final String[] hosts;

  /**
   * @return Host.
   */
  public final String getHost() {
    if ((hosts == null) || (hosts.length == 0)) {
      return null;
    }
    return hosts[0];
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to host).
   */
  public final String getHostURL(boolean secured) {
    String host = getHost();
    if (host == null) {
      return null;
    }
    if (secured && canBeSecured()) {
      return "https://" + host;
    }
    return "http://" + host;
  }

  /**
   * @return List of possible direct paths to article.
   */
  public List<String> getArticleDirectPath() {
    List<String> result = new ArrayList<>();
    if (hosts != null) {
      for (String host : hosts) {
        result.add("//" + host + "/wiki/$1");
      }
    }
    return result;
  }

  /**
   * @return List of possible paths with article named passed as a title parameter.
   */
  public List<String> getArticleParamPath() {
    List<String> result = new ArrayList<>();
    if (hosts != null) {
      for (String host : hosts) {
        result.add("//" + host + "/w/index.php");
      }
    }
    return result;
  }

  /** Path for the API, relative to the host URL */
  private final String apiPath;

  /** API URL (URL to api.php) */
  private final String apiUrl;

  /** Secured API URL (URL to api.php) */
  private final String securedApiUrl;

  /**
   * @return API path.
   */
  public final String getApiPath() {
    return apiPath;
  }

  /**
   * @param secured True if secured connection is requested.
   * @return API URL (URL to api.php).
   */
  public final String getApiURL(boolean secured) {
    if (secured && canBeSecured()) {
      return securedApiUrl;
    }
    return apiUrl;
  }

  /** Path for the index, relative to the host URL */
  private final String indexPath;

  /** Index URL (URL to index.php) */
  private final String indexUrl;

  /** Secured index URL (URL to index.php) */
  private final String securedIndexUrl;

  /**
   * @return Index path.
   */
  public final String getIndexPath() {
    return indexPath;
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Index URL (URL to index.php).
   */
  public final String getIndexURL(boolean secured) {
    if (secured && canBeSecured()) {
      return securedIndexUrl;
    }
    return indexUrl;
  }

  /** Code */
  private final String code;

  /**
   * @return Code.
   */
  public final String getCode() {
    return code;
  }

  /**
   * CheckWiki code.
   */
  private final String codeCheckWiki;

  /**
   * @return CheckWiki code.
   */
  public final String getCodeCheckWiki() {
    return codeCheckWiki;
  }

  /** Text orientation. */
  private final ComponentOrientation orientation;

  /**
   * @return Component orientation.
   */
  public ComponentOrientation getComponentOrientation() {
    return orientation;
  }

  /**
   * @param pageTitle Title.
   * @param redirect Follow redirect ?
   * @param secured True if secured connection is requested.
   * @return URL of the wiki.
   */
  public String getURL(String pageTitle, boolean redirect, boolean secured) {
    try {
      return getIndexURL(secured) +
             "?title=" + URLEncoder.encode(pageTitle, "UTF-8") +
             (redirect ? "" : "&redirect=no");
    } catch (UnsupportedEncodingException e) {
      return getIndexURL(secured);
    }
  }

  /**
   * @param pageTitle Title.
   * @param action Action.
   * @param secured True if secured connection is requested.
   * @return URL of the wiki.
   */
  public String getURL(String pageTitle, String action, boolean secured) {
    try {
      return getIndexURL(secured) +
             "?title=" + URLEncoder.encode(pageTitle, "UTF-8") +
             "&redirect=no" +
             "&action=" + action;
    } catch (UnsupportedEncodingException e) {
      return getIndexURL(secured);
    }
  }

  /**
   * @return Configuration page.
   */
  public String getConfigurationPage() {
    return "WikiCleanerConfiguration";
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return getCode() + " - " + getName();
  }
}
