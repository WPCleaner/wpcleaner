/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionListener;

import javax.swing.AbstractButton;


/**
 * Utilities for actions.
 */
public class ActionUtilities {

  /**
   * Remove all action listeners from a button.
   * 
   * @param button Button.
   */
  public static void removeActionListeners(AbstractButton button) {
    if (button == null) {
      return;
    }
    ActionListener[] listeners = button.getActionListeners();
    if (listeners == null) {
      return;
    }
    for (ActionListener listener : listeners) {
      button.removeActionListener(listener);
    }
  }
}
