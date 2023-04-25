/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://meta.wikimedia.org/w/index.php">Meta wikimedia</a>.
 */
public final class WikimediaMeta extends AbstractWikiSettings {

  /**
   * Constructor.
   */
  public WikimediaMeta() {
    super(
        "meta", "Meta",
        new String[] {
            "meta.wikimedia.org",
        },
        "/w/api.php", "/w/index.php",
        "meta", "metawiki",
        ComponentOrientation.LEFT_TO_RIGHT);
  }
}
