/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;


/**
 * Configuration for <a href="http://commons.wikimedia.org/w/index.php">Commons wikimedia</a>.
 */
public final class WikimediaCommons extends AbstractWikiSettings {

  /**
   * Constructor.
   */
  public WikimediaCommons() {
    super(
        "commons", "Commons",
        "commons.wikimedia.org", "/w/api.php", "/w/index.php",
        "commons", "commons");
  }
}
