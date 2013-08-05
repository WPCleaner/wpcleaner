/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionListener;

import javax.swing.AbstractButton;
import javax.swing.JButton;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;
import org.wikipediacleaner.utils.ConfigurationValueShortcut.ShortcutProperties;


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

  /**
   * @param iconName Icon name.
   * @param showIcon True if the button should use an icon.
   * @param text Text for the button.
   * @param showText True if the button should display the text.
   * @param shortcutType Shortcut.
   * @param useShortcut True if shortcut should be used.
   * @return Button
   */
  static JButton createInternalButton(
      String iconName, boolean showIcon,
      String text, boolean showText,
      ConfigurationValueShortcut shortcutType, boolean useShortcut) {
    JButton button = null;
    ShortcutProperties shortcut = null;
    if (useShortcut && (shortcutType != null)) {
      Configuration config = Configuration.getConfiguration();
      shortcut = config.getShortcut(shortcutType);
    }
    if (showIcon && (iconName != null)) {
      button = Utilities.createJButton(
          iconName, EnumImageSize.NORMAL,
          text, showText, shortcut);
    } else {
      button = Utilities.createJButton(text, shortcut);
    }
    return button;
  }
}
