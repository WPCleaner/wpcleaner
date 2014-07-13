/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://fa.wikipedia.org/w/index.php">Persian wikipedia</a>.
 */
public final class WikipediaFa extends AbstractWikipediaSettings {

  /**
   * Constructor.
   */
  public WikipediaFa() {
    super("fa", "Persian Wikipedia");
  }

  /**
   * @return Component orientation.
   */
  @Override
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.RIGHT_TO_LEFT;
  }
}
