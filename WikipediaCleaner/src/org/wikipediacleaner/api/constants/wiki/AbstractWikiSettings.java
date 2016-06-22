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
   * @param host Host.
   * @param apiPath Path for the API, relative to the host URL.
   * @param indexPath Path for index.php, relative to the host URL.
   * @param code Code for the wiki.
   * @param codeCheckWiki CW code for the wiki.
   * @param orientation Text orientation.
   */
  public AbstractWikiSettings(
      String language, String name,
      String host, String apiPath, String indexPath,
      String code, String codeCheckWiki,
      ComponentOrientation orientation) {
    this.language = language;
    this.name = name;
    this.host = host;
    this.hostUrl = "http://" + host;
    this.securedHostUrl = "https://" + host;
    this.apiPath = apiPath;
    this.apiUrl = hostUrl + apiPath;
    this.securedApiUrl = securedHostUrl + apiPath;
    this.indexPath = indexPath;
    this.indexUrl = hostUrl + indexPath;
    this.securedIndexUrl = securedHostUrl + indexPath;
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

  /** Host */
  private final String host;

  /** Host URL (URL to host) */
  private final String hostUrl;

  /** Secured host URL (URL to host) */
  private final String securedHostUrl;

  /**
   * @return Host.
   */
  public final String getHost() {
    return host;
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to host).
   */
  public final String getHostURL(boolean secured) {
    if (secured && canBeSecured()) {
      return securedHostUrl;
    }
    return hostUrl;
  }

  public List<String> getArticleDirectPath() {
    List<String> result = new ArrayList<>();
    result.add(host + "/wiki/$1");
    return result;
  }

  public List<String> getArticleParamPath() {
    List<String> result = new ArrayList<>();
    result.add(host + "/w/index.php");
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
