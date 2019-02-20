/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

/**
 * An interface for listening to changes in the user name selection.
 */
public interface UserNameChangeListener {

  /**
   * Called when the user name selection has changed.
   * 
   * @param userName New user name.
   */
  public void changeUserName(String userName);
}
