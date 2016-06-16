/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://wiki.waze.com/wiki/">Waze</a>.
 */
public class Waze
  extends AbstractWikiSettings {

  /**
   * Default constructor.
   */
  public Waze() {
    super(
        "en", "Waze",
        "wiki.waze.com", "/wiki/api.php", "/wiki/index.php",
        "waze", null,
        ComponentOrientation.LEFT_TO_RIGHT);
  }
}
