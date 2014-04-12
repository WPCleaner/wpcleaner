/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Abstract base class for Wikisource configuration.
 */
public abstract class AbstractWikisourceSettings
  extends AbstractWikiSettings {

  /**
   * @param language Wikisource language.
   */
  protected AbstractWikisourceSettings(
      String language,
      String name) {
    this.language = language;
    this.code = "s:" + language;
    this.codeCheckWiki = language + "wikisource";
    this.name = name;
    this.hostUrl = "http://" + language + ".wikisource.org";
    this.securedHostUrl = "https://" + language + ".wikisource.org";
    this.apiUrl = hostUrl + "/w/api.php";
    this.securedApiUrl = securedHostUrl + "/w/api.php";
    this.indexUrl = hostUrl + "/w/index.php";
    this.securedIndexUrl = securedHostUrl + "/w/index.php";
  }

  /**
   * Wikisource language.
   */
  private final String language;

  /**
   * @return Wikisource language.
   */
  @Override
  public String getLanguage() {
    return language;
  }

  /**
   * Wikisource code.
   */
  private final String code;

  /**
   * @return Wikisource code.
   */
  @Override
  public String getCode() {
    return code;
  }

  /**
   * CheckWiki code.
   */
  private final String codeCheckWiki;

  /**
   * @return CheckWiki code.
   */
  @Override
  public String getCodeCheckWiki() {
    return codeCheckWiki;
  }

  /**
   * Wikisource name.
   */
  private final String name;

  /**
   * @return Wikisource name.
   */
  @Override
  public String getName() {
    return name;
  }

  /**
   * Host URL (URL to Wikisource host).
   */
  private final String hostUrl;

  /**
   * Secured host URL (URL to Wikisource host).
   */
  private final String securedHostUrl;

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to Wikisource host).
   */
  @Override
  public String getHostURL(boolean secured) {
    if (secured) {
      return securedHostUrl;
    }
    return hostUrl;
  }

  /**
   * API URL (URL to api.php).
   */
  private final String apiUrl;

  /**
   * Secured API URL (URL to api.php).
   */
  private final String securedApiUrl;

  /**
   * @param secured True if secured connection is requested.
   * @return API URL (URL to api.php).
   */
  @Override
  public String getApiURL(boolean secured) {
    if (secured) {
      return securedApiUrl;
    }
    return apiUrl;
  }

  /**
   * Index URL (URL to index.php).
   */
  private final String indexUrl;

  /**
   * Secured index URL (URL to index.php).
   */
  private final String securedIndexUrl;

  /**
   * @param secured True if secured connection is requested.
   * @return Index URL (URL to index.php).
   */
  @Override
  public String getIndexURL(boolean secured) {
    if (secured) {
      return securedIndexUrl;
    }
    return indexUrl;
  }
}