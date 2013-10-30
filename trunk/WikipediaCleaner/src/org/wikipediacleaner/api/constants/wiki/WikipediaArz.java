/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://arz.wikipedia.org/w/index.php">Egyptian Arabic wikipedia</a>.
 */
public final class WikipediaArz extends AbstractWikipediaSettings {

  /**
   * Constructor.
   */
  public WikipediaArz() {
    super("arz", "Egyptian Arabic Wikipedia");
  }

  /**
   * @return Component orientation.
   */
  @Override
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.RIGHT_TO_LEFT;
  }
}
