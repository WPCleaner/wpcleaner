/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Abstract base class for Wiktionary configuration.
 */
public abstract class AbstractWiktionarySettings
  extends AbstractWikiSettings {

  /**
   * @param language Wiktionary language.
   */
  protected AbstractWiktionarySettings(
      String language,
      String name) {
    this.language = language;
    this.code = "wikt:" + language;
    this.codeCheckWiki = language + "wiktionary";
    this.name = name;
    this.hostUrl = "http://" + language + ".wiktionary.org";
    this.securedHostUrl = "https://" + language + ".wiktionary.org";
    this.apiUrl = hostUrl + "/w/api.php";
    this.securedApiUrl = securedHostUrl + "/w/api.php";
    this.indexUrl = hostUrl + "/w/index.php";
    this.securedIndexUrl = securedHostUrl + "/w/index.php";
  }

  /**
   * Wiktionary language.
   */
  private final String language;

  /**
   * @return Wiktionary language.
   */
  @Override
  public String getLanguage() {
    return language;
  }

  /**
   * Wiktionary code.
   */
  private final String code;

  /**
   * @return Wiktionary code.
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
   * Wiktionary name.
   */
  private final String name;

  /**
   * @return Wiktionary name.
   */
  @Override
  public String getName() {
    return name;
  }

  /**
   * Host URL (URL to Wiktionary host).
   */
  private final String hostUrl;

  /**
   * Secured host URL (URL to Wiktionary host).
   */
  private final String securedHostUrl;

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to Wiktionary host).
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
