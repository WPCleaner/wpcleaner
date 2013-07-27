/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;


/**
 * An interface used to listen to BasicWindow events.
 */
public interface BasicWindowListener {

  /**
   * Called just after BasicWindow constructor has been called.
   * 
   * @param window BasicWindow.
   */
  public void initializeWindow(BasicWindow window);

  /**
   * Called just after BasicWindow has been displayed.
   * 
   * @param window BasicWindow.
   */
  public void displayWindow(BasicWindow window);
}
