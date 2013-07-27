/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://ar.wikipedia.org/w/index.php">Arabic wikipedia</a>.
 */
public final class WikipediaAr extends AbstractWikipediaSettings {

  /**
   * Constructor.
   */
  public WikipediaAr() {
    super("ar", "Arabic Wikipedia");
  }

  /**
   * @return Component orientation.
   */
  @Override
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.RIGHT_TO_LEFT;
  }
}
