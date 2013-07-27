/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://yi.wikipedia.org/w/index.php">Yiddish wikipedia</a>.
 */
public final class WikipediaYi extends AbstractWikipediaSettings {

  /**
   * Constructor.
   */
  public WikipediaYi() {
    super("yi", "Yiddish Wikipedia");
  }

  /**
   * @return Component orientation.
   */
  @Override
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.RIGHT_TO_LEFT;
  }
}
