/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Abstract base class for Wikiversity configuration.
 */
public abstract class AbstractWikiversitySettings
  extends AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   */
  protected AbstractWikiversitySettings(
      String language,
      String name) {
    super(
        language, name,
        language + ".wikiversity.org", "/w/api.php", "/w/index.php",
        "v:" + language, language + "wikiversity");
  }
}
