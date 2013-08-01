/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.Window;
import java.awt.event.ActionListener;
import java.beans.EventHandler;

import javax.swing.JButton;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;
import org.wikipediacleaner.utils.ConfigurationValueShortcut.ShortcutProperties;


/**
 * Manage actions for disposing of a window.
 */
public class ActionDispose {

  /**
   * @param useShortcut True if shortcut should be used.
   * @param cancel True if it should be a cancel button.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean useShortcut, boolean cancel) {
    JButton button = null;
    ShortcutProperties shortcut = null;
    if (useShortcut) {
      Configuration config = Configuration.getConfiguration();
      shortcut = config.getShortcut(ConfigurationValueShortcut.CLOSE);
    }
    button = Utilities.createJButton(
        cancel ? GT._("Cancel") : GT._("Close"), shortcut);
    return button;
  }

  /**
   * Create a button for disposing of a window.
   * 
   * @param window Window.
   * @param useShortcut True if shortcut should be used.
   * @param cancel True if it should be a cancel button.
   * @return Button.
   */
  public static JButton createButton(
      Window window, boolean useShortcut, boolean cancel) {
    JButton button = createInternalButton(useShortcut, cancel);
    button.addActionListener(EventHandler.create(ActionListener.class, window, "dispose"));
    return button;
  }
}
