/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Abstract base class for Wikiquote configuration.
 */
public abstract class AbstractWikiquoteSettings
  extends AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   */
  protected AbstractWikiquoteSettings(
      String language,
      String name) {
    super(
        language, name,
        language + ".wikiquote.org", "/w/api.php", "/w/index.php",
        "q:" + language, language + "wikiquote");
  }
}
