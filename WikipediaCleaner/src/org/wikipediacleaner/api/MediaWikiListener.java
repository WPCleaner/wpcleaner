/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;


/**
 * A listener of MediaWiki events. 
 */
public interface MediaWikiListener {

  /**
   * @param text Text to display.
   */
  public void setText(String text);

  /**
   * @return Flag indicating if the process should stop.
   */
  public boolean shouldStop();
}
