/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner;

import java.util.Locale;

import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.gui.swing.InstallerWindow;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * Installer for WPCleaner.
 */
public class Installer {

  /**
   * @param args Command line arguments.
   */
  public static void main(String[] args) {

    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = EnumLanguage.getDefaultLanguage();
    Locale.setDefault(language.getLocale());

    // Look & Feel
    String lookAndFeelClassName = null;
    lookAndFeelClassName = getLookAndFeelClassName("Nimbus");
    if (lookAndFeelClassName != null) {
      try {
        UIManager.setLookAndFeel(lookAndFeelClassName);
      } catch (ClassNotFoundException e) {
        // Not important
      } catch (InstantiationException e) {
        // Not important
      } catch (IllegalAccessException e) {
        // Not important
      } catch (UnsupportedLookAndFeelException e) {
        // Not important
      }
    }

    // Language
    GT.setCurrentLanguage(config.getLanguage());

    // Analyze command line arguments
    // int currentArg = 0;

    // Running
    InstallerWindow.createInstallerWindow();
  }

  /**
   * @param name Look and Feel name
   * @return Look and Feel class name
   */
  private static String getLookAndFeelClassName(String name) {
    if (name == null) {
      return null;
    }
    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
      if (name.equals(info.getName())) {
        return info.getClassName();
      }
    }
    return null;
  }
}
