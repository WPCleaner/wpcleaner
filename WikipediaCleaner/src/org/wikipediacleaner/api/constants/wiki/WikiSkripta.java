/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Configuration for <a href="http://www.wikiskripta.eu/index.php/Home">WikiSkripta</a>.
 */
public class WikiSkripta
  extends AbstractWikiSettings {

  /**
   * Default constructor.
   */
  public WikiSkripta() {
    //
  }

  /**
   * @return Wikipedia language.
   */
  @Override
  public String getLanguage() {
    return "cs";
  }

  /**
   * @return Wikipedia code.
   */
  @Override
  public String getCode() {
    return "wikiskripta";
  }

  /**
   * @return CheckWiki code.
   */
  @Override
  public String getCodeCheckWiki() {
    return null;
  }

  /**
   * @return Wikipedia name.
   */
  @Override
  public String getName() {
    return "WikiSkripta";
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to Wikipedia host).
   */
  @Override
  public String getHostURL(boolean secured) {
    if (secured) {
      return "http://www.wikiskripta.eu";
    }
    return "http://www.wikiskripta.eu";
  }

  /**
   * @param secured True if secured connection is requested.
   * @return API URL (URL to api.php).
   */
  @Override
  public String getApiURL(boolean secured) {
    if (secured) {
      return "http://www.wikiskripta.eu/api.php";
    }
    return "http://www.wikiskripta.eu/api.php";
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Index URL (URL to index.php).
   */
  @Override
  public String getIndexURL(boolean secured) {
    if (secured) {
      return "http://www.wikiskripta.eu/index.php";
    }
    return "http://www.wikiskripta.eu/index.php";
  }
}
