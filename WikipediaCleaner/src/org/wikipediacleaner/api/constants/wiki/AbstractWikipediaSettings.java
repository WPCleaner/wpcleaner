/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Abstract base class for Wikipedia configuration.
 */
public abstract class AbstractWikipediaSettings
  extends AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   */
  protected AbstractWikipediaSettings(
      String language,
      String name) {
    super(
        language, name,
        language + ".wikipedia.org", "/w/api.php", "/w/index.php",
        language, language + "wiki");
  }
}
