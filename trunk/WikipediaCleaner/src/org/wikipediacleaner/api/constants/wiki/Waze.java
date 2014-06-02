/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Configuration for <a href="http://wiki.waze.com/wiki/">Waze</a>.
 */
public class Waze
  extends AbstractWikiSettings {

  /**
   * @param language Wikipedia language.
   */
  public Waze() {
    //
  }

  /**
   * @return Wikipedia language.
   */
  @Override
  public String getLanguage() {
    return "en";
  }

  /**
   * @return Wikipedia code.
   */
  @Override
  public String getCode() {
    return "waze";
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
    return "Waze";
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to Wikipedia host).
   */
  @Override
  public String getHostURL(boolean secured) {
    if (secured) {
      return "https://wiki.waze.com";
    }
    return "http://wiki.waze.com";
  }

  /**
   * @param secured True if secured connection is requested.
   * @return API URL (URL to api.php).
   */
  @Override
  public String getApiURL(boolean secured) {
    if (secured) {
      return "https://wiki.waze.com/wiki/api.php";
    }
    return "http://wiki.waze.com/wiki/api.php";
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Index URL (URL to index.php).
   */
  @Override
  public String getIndexURL(boolean secured) {
    if (secured) {
      return "https://wiki.waze.com/w/index.php";
    }
    return "http://wiki.waze.com/w/index.php";
  }
}
