/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;


/**
 * A listener for the insertion of predefined text.
 */
public interface ListenerPredefinedTextInsertion {

  /**
   * Notification of the insertion of a category.
   * 
   * @param categoryName Name of the category.
   */
  public void categoryInserted(String categoryName);

  /**
   * Notification of the insertion of a template.
   * 
   * @param templateName Name of the template.
   */
  public void templateInserted(String templateName);
}
