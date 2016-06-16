/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Class for Wikiversity configuration.
 */
public class Wikiversity
  extends AbstractWikiSettings {

  /**
   * @param language Language.
   * @param name Name.
   */
  public Wikiversity(
      String language,
      String name) {
    this(language, name, ComponentOrientation.LEFT_TO_RIGHT);
  }

  /**
   * @param language Language.
   * @param name Name.
   * @param orientation Text orientation.
   */
  public Wikiversity(
      String language,
      String name,
      ComponentOrientation orientation) {
    super(
        language, name,
        language + ".wikiversity.org", "/w/api.php", "/w/index.php",
        "v:" + language, language + "wikiversity",
        orientation);
  }
}
