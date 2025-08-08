/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner;

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import javax.swing.InputMap;
import javax.swing.KeyStroke;
import javax.swing.RepaintManager;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.FontUIResource;
import javax.swing.text.DefaultEditorKit;

import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.ISBNRange;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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

  /** Logger */
  private final static org.slf4j.Logger log = LoggerFactory.getLogger(WikipediaCleaner.class);

  private final static boolean CHECK_EDT = true;
  private final static boolean SYSTEM_LF = true;

  /**
   * @param args Command line arguments.
   */
  public static void main(String[] args) {
    try {
      internalMain(args);
    } catch (Throwable t) {
      log.error("Error running WPCleaner", t);
    }
  }

  /**
   * @param args Command line arguments
   */
  private static void internalMain(String[] args) {

    // Log levels
    Logger.getLogger("org.lobobrowser").setLevel(Level.WARNING);
    Logger.getLogger("").setLevel(Level.WARNING);
    Logger.getLogger("org.wikipediacleaner").setLevel(Level.INFO);

    log.info("Starting WPCleaner");

    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = EnumLanguage.getDefaultLanguage();
    Locale.setDefault(language.getLocale());

    // Debugging
    if (config.getBoolean(null, ConfigurationValueBoolean.DEBUG_DETAILS)) {
      Logger.getLogger("org.wikipediacleaner").setLevel(Level.FINE);
      PageAnalysis.setTraceTime(true);
      AlgorithmError.setTraceTime(true);
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

    // Check that calls are made in the Event Dispatch Thread
    if (CHECK_EDT) {
      RepaintManager.setCurrentManager(new CheckThreadViolationRepaintManager());
    }

    // Look & Feel
    String lookAndFeelClassName = null;
    if (SYSTEM_LF) {
      lookAndFeelClassName = UIManager.getSystemLookAndFeelClassName();
    }
    lookAndFeelClassName = switch (config.getInt(null, ConfigurationValueInteger.PLAF_TYPE)) {
      case ConfigurationConstants.VALUE_PLAF_TYPE_WPCLEANER -> getLookAndFeelClassName("Nimbus");
      case ConfigurationConstants.VALUE_PLAF_TYPE_USER -> getLookAndFeelClassName(
          config.getString(null, ConfigurationValueString.PLAF_NAME));
      default -> lookAndFeelClassName;
    };
    if (lookAndFeelClassName != null) {
      try {
        UIManager.setLookAndFeel(lookAndFeelClassName);
      } catch (ClassNotFoundException | InstantiationException | IllegalAccessException |
               UnsupportedLookAndFeelException e) {
        // Not important
      }
    }

    // Font size for defaults
    String fontName = config.getString(null, ConfigurationValueString.FONT_NAME_OTHER);
    if (fontName != null) {
      int fontSize = config.getInt(null, ConfigurationValueInteger.FONT_SIZE);
      Arrays.stream(GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts())
          .filter(font -> fontName.equals(font.getFontName()))
          .findFirst()
          .ifPresent(font -> {
            replaceFonts(UIManager.getLookAndFeelDefaults(), font, fontSize);
            replaceFonts(UIManager.getDefaults(), font, fontSize);
          });
    }

    // Manage copy/paste for OS X
    int menuShortcut = Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx();
    if (menuShortcut == InputEvent.META_DOWN_MASK) {
      InputMap im = (InputMap) UIManager.get("TextField.focusInputMap");
      if (im != null) {
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, menuShortcut), DefaultEditorKit.copyAction);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, menuShortcut), DefaultEditorKit.pasteAction);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, menuShortcut), DefaultEditorKit.cutAction);
      }
    }

    // Language
    GT.setCurrentLanguage(config.getLanguage());

    // Various initializations
    ISBNRange.initialize();

    // Analyze command line arguments
    int currentArg = 0;
    int previousArg = currentArg - 1;
    String credentials = null;
    String wikiCode = null;
    while (previousArg < currentArg) {
      previousArg = currentArg;

      // Credentials
      if ((args.length > currentArg + 1) &&
          ("-credentials".equalsIgnoreCase(args[currentArg]))) {
        credentials = args[currentArg + 1];
        currentArg += 2;
      }

      // Wiki
      if ((args.length > currentArg + 1) &&
          ("-wiki".equalsIgnoreCase(args[currentArg]))) {
        wikiCode = args[currentArg + 1];
        currentArg += 2;
      }
    }

    // Retrieve wiki
    if ((wikiCode == null) && (args.length > currentArg)) {
      wikiCode = args[currentArg];
    }
    EnumWikipedia wiki = (wikiCode != null) ? EnumWikipedia.getWikipedia(wikiCode) : null;
    currentArg++;

    // Retrieve username and password
    String userName = null;
    String password = null;
    if (credentials != null) {
      Properties properties = new Properties();
      try (BufferedReader reader = new BufferedReader(new InputStreamReader(
          new FileInputStream(credentials), StandardCharsets.UTF_8))) {
        properties.load(reader);
      } catch (IOException e) {
        // Doing nothing
      }
      userName = properties.getProperty("user");
      password = properties.getProperty("password");
    } else {
      if (args.length > currentArg) {
        userName = args[currentArg];
      }
      currentArg++;
      // Retrieve password
      if (args.length > currentArg) {
        password = args[currentArg];
      }
      currentArg++;
    }

    // Running
    log.info("Display main window");
    MainWindow.createMainWindow(wiki, userName, password);
  }

  private static void replaceFonts(UIDefaults defaults, Font font, int fontSize) {
    Enumeration<Object> keys = defaults.keys();
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = defaults.get(key);
      if (value instanceof FontUIResource resource) {
        Font newFont = font.deriveFont(resource.getStyle()).deriveFont((float) resource.getSize() + fontSize);
        defaults.put(key, new FontUIResource(newFont));
      }
    }
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
