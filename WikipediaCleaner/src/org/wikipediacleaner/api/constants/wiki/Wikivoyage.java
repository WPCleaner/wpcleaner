/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Class for Wikivoyage configuration.
 */
public class Wikivoyage
  extends AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   */
  public Wikivoyage(
      String language,
      String name) {
    this(language, name, ComponentOrientation.LEFT_TO_RIGHT);
  }

  /**
   * @param language Language.
   * @param name Name.
   * @param orientation Text orientation.
   */
  public Wikivoyage(
      String language,
      String name,
      ComponentOrientation orientation) {
    super(
        language, name,
        new String[] {
            language + ".wikivoyage.org",
        },
        "/w/api.php", "/w/index.php",
        "voy:" + language, language + "wikivoyage",
        orientation);
  }
}
