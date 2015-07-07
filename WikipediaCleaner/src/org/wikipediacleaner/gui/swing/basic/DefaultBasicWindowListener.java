/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;


/**
 * Default basic window creation callbacks.
 */
public abstract class DefaultBasicWindowListener implements BasicWindowListener {

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindowCreation#initializeWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
   */
  @Override
  public void initializeWindow(
      @SuppressWarnings("unused") BasicWindow window) {
    //
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindowCreation#displayWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
   */
  @Override
  public void displayWindow(
      @SuppressWarnings("unused") BasicWindow window) {
    //
  }
}
