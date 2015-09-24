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
   * @param language Language.
   * @param name Name.
   */
  protected AbstractWiktionarySettings(
      String language,
      String name) {
    super(
        language, name,
        language + ".wiktionary.org", "/w/api.php", "/w/index.php",
        "wikt:" + language, language + "wiktionary");
  }
}
