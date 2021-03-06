/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Class for Wiktionary configuration.
 */
public class Wiktionary
  extends AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   */
  public Wiktionary(
      String language,
      String name) {
    this(language, name, ComponentOrientation.LEFT_TO_RIGHT);
  }

  /**
   * @param language Language.
   * @param name Name.
   * @param orientation Text orientation.
   */
  public Wiktionary(
      String language,
      String name,
      ComponentOrientation orientation) {
    super(
        language, name,
        new String[] {
            language + ".wiktionary.org",
        },
        "/w/api.php", "/w/index.php",
        "wikt:" + language, language + "wiktionary",
        orientation);
  }
}
