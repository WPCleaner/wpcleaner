/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Configuration for <a href="http://commons.wikimedia.org/w/index.php">Commons wikimedia</a>.
 */
public final class WikimediaCommons extends AbstractWikiSettings {

  /**
   * Constructor.
   */
  public WikimediaCommons() {
    //
  }

  /**
   * @return Language.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getLanguage()
   */
  @Override
  public String getLanguage() {
    return "commons";
  }

  /**
   * @return Code.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getCode()
   */
  @Override
  public String getCode() {
    return "commons";
  }

  /**
   * @return Code for Check Wiki.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getCodeCheckWiki()
   */
  @Override
  public String getCodeCheckWiki() {
    return "commons";
  }

  /**
   * @return Name.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getName()
   */
  @Override
  public String getName() {
    return "Commons";
  }

  /**
   * @param secured True if secured URL should be returned.
   * @return Host URL.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getHostURL(boolean)
   */
  @Override
  public String getHostURL(boolean secured) {
    return "https://commons.wikimedia.org";
  }

  /**
   * @param secured True if secured URL should be returned.
   * @return API URL.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getApiURL(boolean)
   */
  @Override
  public String getApiURL(boolean secured) {
    return "https://commons.wikimedia.org/w/api.php";
  }

  /**
   * @param secured True if secured URL should be returned.
   * @return URL for index.php.
   * @see org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings#getIndexURL(boolean)
   */
  @Override
  public String getIndexURL(boolean secured) {
    return "https://commons.wikimedia.org/w/index.php";
  }
}
