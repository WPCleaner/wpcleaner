/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner;

import java.util.Arrays;
import java.util.Locale;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.BasicWorkerListener;
import org.wikipediacleaner.gui.swing.worker.LoginWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateISBNWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Wikipedia Cleaner running as a bot.
 */
public class Bot {

  private final static Log log = LogFactory.getLog(Bot.class);

  /**
   * @param args
   */
  public static void main(String[] args) {
    System.out.println("Running as bot");

    // Log levels
    Logger.getLogger("org.lobobrowser").setLevel(Level.WARNING);
    Logger.getLogger("").setLevel(Level.WARNING);

    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = config.getLanguage();
    Locale.setDefault(language.getLocale());
    System.out.println("Language: " + language);

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
    System.out.println("Debugging configured");

    // Language
    GT.setCurrentLanguage(config.getLanguage());
    System.out.println("Language configured");

    new Bot(args);
  }

  private EnumWikipedia wiki;

  /**
   * @param args Command line arguments
   */
  private Bot(String[] args) {

    // Analyze command line arguments
    int currentArg = 0;

    // Retrieve wiki
    if (args.length > currentArg) {
      String wikiCode = args[currentArg];
      wiki = EnumWikipedia.getWikipedia(wikiCode);
      if ((wiki == null) || !wikiCode.equals(wiki.getSettings().getCode())) {
        log.warn("Unable to find wiki " + wikiCode);
        return;
      }
    }
    log.info("Wiki: " + wiki);
    currentArg++;

    // Retrieve user name
    String userName = null;
    if (args.length > currentArg) {
      userName = args[currentArg];
    }
    currentArg++;

    // Retrieve password
    String password = null;
    if (args.length > currentArg) {
      password = args[currentArg];
    }
    currentArg++;

    // Retrieve action
    final String[] actions = (args.length > currentArg) ?
        Arrays.copyOfRange(args, currentArg, args.length) : null;
    currentArg++;

    // Check arguments
    if ((wiki == null) ||
        (userName == null) ||
        (password == null) ||
        (actions == null) ||
        (actions.length == 0)) {
      return;
    }

    // Login
    LoginWorker loginWorker = new LoginWorker(
        wiki, null, null, EnumLanguage.getDefaultLanguage(),
        userName, password.toCharArray(),
        ConfigurationConstants.VALUE_SAVE_USER_NO_CHANGE, true, false);
    loginWorker.setListener(new BasicWorkerListener() {

      /**
       * Called just at the beginning of the start() method in BasicWorker.
       * 
       * @param worker Current worker.
       */
      public void beforeStart(BasicWorker worker) {
        // Nothing to do
      }

      /**
       * Called just at the end of the start() method in BasicWorker.
       * 
       * @param worker Current worker.
       */
      public void afterStart(BasicWorker worker) {
        // Nothing to do
      }

      /**
       * Called just at the beginning of the finished() method in BasicWorker.
       * 
       * @param worker Current worker.
       */
      public void beforeFinished(BasicWorker worker) {
        // Nothing to do
      }

      /**
       * Called just at the end of the finished() method in BasicWorker.
       * 
       * @param worker Current worker.
       * @param ok Flag indicating if the worker finished OK.
       */
      public void afterFinished(BasicWorker worker, boolean ok) {
        if (ok) {
          executeAction(actions);
        }
      }
    });
    loginWorker.start();
  }

  /**
   * Execute an action.
   * 
   * @param args Action and arguments.
   */
  void executeAction(String[] args) {

    // Retrieve action
    int currentArg = 0;
    if (currentArg >= args.length) {
      return;
    }
    String action = args[0];
    currentArg++;

    // Execute action depending on the parameters
    if ("UpdateISBNWarnings".equalsIgnoreCase(action)) {
      UpdateISBNWarningWorker worker = new UpdateISBNWarningWorker(wiki, null, false);
      worker.start();
    }
  }
}
