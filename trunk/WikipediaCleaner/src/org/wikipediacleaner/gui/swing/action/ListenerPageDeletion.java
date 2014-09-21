/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;


/**
 * A listener for the deletion of a page.
 */
public interface ListenerPageDeletion {

  /**
   * Notification of the deletion of a page.
   * 
   * @param pageName Name of the page.
   */
  public void pageDeleted(String pageName);
}
