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
   * @param language Language.
   * @param name Name.
   */
  protected AbstractWikisourceSettings(
      String language,
      String name) {
    super(
        language, name,
        language + ".wikisource.org", "/w/api.php", "/w/index.php",
        "s:" + language, language + "wikisource");
  }
}
