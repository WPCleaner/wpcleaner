/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://he.wikipedia.org/w/index.php">Hebrew wikipedia</a>.
 */
public final class WikipediaHe extends AbstractWikipediaSettings {

  /**
   * Constructor.
   */
  public WikipediaHe() {
    super("he", "ויקיפדיה העברית");
  }

  /**
   * @return Component orientation.
   */
  @Override
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.RIGHT_TO_LEFT;
  }
}
