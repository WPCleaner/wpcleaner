/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner;

import java.awt.Font;
import java.util.Locale;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import javax.swing.RepaintManager;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import org.wikipediacleaner.api.check.ISBNRange;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.gui.swing.MainWindow;
import org.wikipediacleaner.gui.swing.component.CheckThreadViolationRepaintManager;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Wikipedia Cleaner.
 */
public class WikipediaCleaner {

  private final static boolean CHECK_EDT = false;
  private final static boolean SYSTEM_LF = true;

  /**
   * @param args
   */
  public static void main(String[] args) {

    // Log levels
    Logger.getLogger("org.lobobrowser").setLevel(Level.WARNING);
    Logger.getLogger("").setLevel(Level.WARNING);

    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = EnumLanguage.getDefaultLanguage();
    Locale.setDefault(language.getLocale());

    // Check that calls are made in the Event Dispatch Thread
    if (CHECK_EDT) {
      RepaintManager.setCurrentManager(new CheckThreadViolationRepaintManager());
    }

    // Look & Feel
    String lookAndFeelClassName = null;
    if (SYSTEM_LF) {
      lookAndFeelClassName = UIManager.getSystemLookAndFeelClassName();
    }
    switch (config.getInt(null, ConfigurationValueInteger.PLAF_TYPE)) {
    case ConfigurationConstants.VALUE_PLAF_TYPE_WPCLEANER:
      lookAndFeelClassName = getLookAndFeelClassName("Nimbus");
      break;
    case ConfigurationConstants.VALUE_PLAF_TYPE_USER:
      lookAndFeelClassName = getLookAndFeelClassName(
          config.getString(null, ConfigurationValueString.PLAF_NAME));
      break;
    }
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

    // Font size for defaults
    int fontSize = config.getInt(null, ConfigurationValueInteger.FONT_SIZE);
    if (fontSize > 0) {
      UIDefaults defaults = UIManager.getLookAndFeelDefaults();
      for (Object key : defaults.keySet()) {
        Font font = defaults.getFont(key);
        if (font != null) {
          font = font.deriveFont((float) (font.getSize() + fontSize));
          defaults.put(key, font);
        }
      }
    }

    // Debugging
    if (config.getBoolean(null, ConfigurationValueBoolean.DEBUG_DETAILS)) {
      Logger.getLogger("org.wikipediacleaner").setLevel(Level.FINE);
    }
    if (config.getBoolean(null, ConfigurationValueBoolean.DEBUG_FILE)) {
      try {
        Handler fh = new FileHandler("%t/WPCleaner.log");
        fh.setFormatter(new SimpleFormatter());
        Logger.getLogger("").addHandler(fh);
      } catch (Exception e) {
        // Nothing to do
      }
    }

    // Language
    GT.setCurrentLanguage(config.getLanguage());

    // Various initializations
    ISBNRange.initialize();

    // Running
    MainWindow.createMainWindow();
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
