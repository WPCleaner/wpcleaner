/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import org.wikipediacleaner.api.constants.EnumWikipedia;

/**
 * An interface for listening to changes in the wiki selection.
 */
public interface WikiChangeListener {

  /**
   * Called when the wiki selection has changed.
   * 
   * @param wiki New wiki.
   */
  public void changeWiki(EnumWikipedia wiki);
}
