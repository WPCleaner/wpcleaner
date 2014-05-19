/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.BasicWorkerListener;
import org.wikipediacleaner.gui.swing.bot.AutomaticCWWorker;
import org.wikipediacleaner.gui.swing.worker.LoginWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateISBNWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Wikipedia Cleaner running as a bot.
 */
public class Bot implements BasicWorkerListener {

  private final static Log log = LogFactory.getLog(Bot.class);

  /**
   * @param args
   */
  public static void main(String[] args) {
    log.info("Running as bot");

    // Log levels
    Logger.getLogger("org.lobobrowser").setLevel(Level.WARNING);
    Logger.getLogger("").setLevel(Level.WARNING);

    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = config.getLanguage();
    Locale.setDefault(language.getLocale());

    // Debugging
    if (config.getBoolean(null, ConfigurationValueBoolean.DEBUG_DETAILS)) {
      Logger.getLogger("org.wikipediacleaner").setLevel(Level.FINE);
    }
    if (config.getBoolean(null, ConfigurationValueBoolean.DEBUG_FILE)) {
      try {
        Handler fh = new FileHandler("WPCleanerBot.log");
        fh.setFormatter(new SimpleFormatter());
        Logger.getLogger("").addHandler(fh);
      } catch (Exception e) {
        // Nothing to do
      }
    }

    // Language
    GT.setCurrentLanguage(config.getLanguage());

    new Bot(args);
  }

  /** Wiki */
  private EnumWikipedia wiki;

  /** True if login is done */
  private boolean loginDone;

  /** Actions to be executed */
  private String[] actions;

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
    actions = (args.length > currentArg) ?
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
    loginDone = false;
    LoginWorker loginWorker = new LoginWorker(
        wiki, null, null, EnumLanguage.getDefaultLanguage(),
        userName, password.toCharArray(),
        ConfigurationConstants.VALUE_SAVE_USER_NO_CHANGE, true, false);
    loginWorker.setListener(this);
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
      worker.setListener(this);
      worker.start();
    } else if ("ListISBNWarnings".equalsIgnoreCase(action)) {
      UpdateISBNWarningWorker worker = new UpdateISBNWarningWorker(wiki, null, true);
      worker.setListener(this);
      worker.start();
    } else if ("FixCheckWiki".equalsIgnoreCase(action)) {
      List<CheckErrorAlgorithm> algorithms = new ArrayList<CheckErrorAlgorithm>();
      for (int i = 1; i < args.length; i++) {
        CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, Integer.parseInt(args[i]));
        if (algorithm != null) {
          algorithms.add(algorithm);
        }
      }
      AutomaticCWWorker worker = new AutomaticCWWorker(
          wiki, null, algorithms, 10000, algorithms, null, true, false);
      worker.setListener(this);
      worker.start();
    }
  }

  /**
   * @param worker
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#beforeStart(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  public void beforeStart(BasicWorker worker) {
    // Do nothing
  }

  /**
   * @param worker
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#afterStart(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  public void afterStart(BasicWorker worker) {
    // Do nothing
  }

  /**
   * @param worker
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#beforeFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  public void beforeFinished(BasicWorker worker) {
    // Do nothing
  }

  /**
   * @param worker
   * @param ok
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#afterFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker, boolean)
   */
  public void afterFinished(BasicWorker worker, boolean ok) {
    if (!ok) {
      System.exit(1);
    }
    if (loginDone) {
      System.exit(0);
    }
    loginDone = true;
    executeAction(actions);
  }
}
