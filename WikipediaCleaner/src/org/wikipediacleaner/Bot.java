/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.StringJoiner;

import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a501.CheckErrorAlgorithm501;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.ISBNRange;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.impl.CommentManager;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.BasicWorkerListener;
import org.wikipediacleaner.gui.swing.bot.AutomaticCWWorker;
import org.wikipediacleaner.gui.swing.bot.AutomaticFileCWWorker;
import org.wikipediacleaner.gui.swing.bot.AutomaticLintErrorWorker;
import org.wikipediacleaner.gui.swing.bot.AutomaticListCWWorker;
import org.wikipediacleaner.gui.swing.bot.FixDumpWorker;
import org.wikipediacleaner.gui.swing.bot.ListCWWorker;
import org.wikipediacleaner.gui.swing.worker.LoginWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDuplicateArgsWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateISBNWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateISSNWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateUnknownParameterWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Wikipedia Cleaner running as a bot.
 */
public class Bot implements BasicWorkerListener {

  private final static Logger log = LoggerFactory.getLogger(Bot.class);

  /**
   * @param args Command line arguments.
   */
  public static void main(String[] args) {
    log.info("Running as bot");

    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = config.getLanguage();
    Locale.setDefault(language.getLocale());

    // Language
    GT.setCurrentLanguage(config.getLanguage());

    // Various initializations
    ISBNRange.initialize();

    new Bot(args);
  }

  /** Time limit for bot execution. */
  private Long timeLimit;

  /** Wiki */
  private EnumWikipedia wiki;

  /** True if login is done */
  private boolean loginDone;

  /** Actions to be executed */
  private List<Action> actions;

  /** List of additional algorithms */
  private List<CheckErrorAlgorithm> additionalAlgorithms;

  /** List of namespaces */
  private Set<Integer> namespaces;

  /** To work only on pages with titles after the beginning of the range */
  private String rangeBegin;

  /** To work only on pages with titles before the end of the range */
  private String rangeEnd;

  /** List of groups that are used for fixing typography and spelling errors (CW#501) */
  private final Set<String> typoGroups = new HashSet<>();

  /**
   * @param args Command line arguments
   */
  private Bot(String[] args) {

    // Analyze command line arguments
    int currentArg = 0;

    // Process general command line arguments
    boolean done = false;
    timeLimit = null;
    String credentials = null;
    String prefix = null;
    while (!done) {
      if (args.length > currentArg) {
        String arg = args[currentArg];

        if ("-timelimit".equals(arg)) {
          timeLimit = Long.valueOf(System.currentTimeMillis() + 1000 * Integer.parseInt(args[currentArg + 1]));
          currentArg += 2;
        } else if ("-credentials".equals(arg)) {
          if (args.length <= currentArg + 1) {
            log.warn("When using parameter '-credentials', you must specify the file containing the credentials");
            return;
          }
          credentials = args[currentArg + 1];
          currentArg += 2;
        } else if ("-prefix".equals(arg)) {
          if (args.length <= currentArg + 1) {
            log.warn("When using parameter '-prefix', you must specify the prefix used for the comments");
            return;
          }
          prefix = args[currentArg + 1].replaceAll("_", " ");
          currentArg += 2;
        } else {
          done = true;
        }
      }
    }
    if (prefix != null) {
      CommentManager.addExtraText(prefix);
    }

    // Retrieve wiki
    if (args.length > currentArg) {
      String wikiCode = args[currentArg];
      wiki = EnumWikipedia.getWikipedia(wikiCode);
      if ((wiki == null) || !wikiCode.equals(wiki.getSettings().getCode())) {
        log.warn("Unable to find wiki {}", wikiCode);
        return;
      }
    }
    currentArg++;

    // Retrieve user name and password
    String userName = null;
    String password = null;
    if (credentials != null) {
      Properties properties = new Properties();
      try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(credentials), "UTF8"))) {
        properties.load(reader);
      } catch (IOException e) {
        log.warn("Unable to load credentials file {}", credentials);
      }
      userName = properties.getProperty("user");
      password = properties.getProperty("password");
    } else {
      if (args.length > currentArg) {
        userName = args[currentArg];
      }
      currentArg++;
      if (args.length > currentArg) {
        password = args[currentArg];
      }
      currentArg++;
    }

    // Retrieve action
    actions = new ArrayList<>();
    if (args.length > currentArg) {
      actions.add(new Action(Arrays.copyOfRange(args, currentArg, args.length), null));
    }
    currentArg++;

    // Check arguments
    if ((wiki == null) ||
        (userName == null) ||
        (password == null) ||
        actions.isEmpty() ||
        !actions.get(0).isOk()) {
      log.warn("Some parameters are incorrect");
      return;
    }

    // Initialization
    additionalAlgorithms = new ArrayList<>();
    namespaces = Collections.singleton(Namespace.MAIN);

    // Check Java version
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8)) {
      log.error(
          "WPCleaner requires Java {} or above, but you're currently using an older version ({}).",
          "8", System.getProperty("java.version"));
      try {
        Thread.sleep(30000);
      } catch (InterruptedException e) {
        // Nothing to do
      }
      System.exit(1);
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
   * @param actionConfig Action and its configuration.
   */
  void executeAction(Action actionConfig) {

    // Retrieve action
    String action = actionConfig.action;
    if (action == null) {
      return;
    }

    // Execute action depending on the parameters
    BasicWorker worker = null;
    boolean actionDone = false;
    if (action.startsWith("#")) {
      actionDone = true;
    } else if ("DoTasks".equalsIgnoreCase(action)) {
      actionDone = executeDoTasks(actionConfig);
    } else if ("UpdateDabWarnings".equalsIgnoreCase(action)) {
      worker = executeUpdateDabWarning(actionConfig);
    } else if ("UpdateISBNWarnings".equalsIgnoreCase(action)) {
      worker = new UpdateISBNWarningWorker(wiki, null, false);
    } else if ("ListISBNWarnings".equalsIgnoreCase(action)) {
      worker = new UpdateISBNWarningWorker(wiki, null, true);
    } else if ("UpdateISSNWarnings".equalsIgnoreCase(action)) {
      worker = new UpdateISSNWarningWorker(wiki, null, false);
    } else if ("ListISSNWarnings".equalsIgnoreCase(action)) {
      worker = new UpdateISSNWarningWorker(wiki, null, true);
    } else if ("UpdateDuplicateArgsWarnings".equalsIgnoreCase(action)) {
      worker = new UpdateDuplicateArgsWarningWorker(wiki, null, false);
    } else if ("UpdateUnknownParameterWarnings".equalsIgnoreCase(action)) {
      worker = new UpdateUnknownParameterWarningWorker(wiki, null, false);
    } else if ("FixCheckWiki".equalsIgnoreCase(action)) {
      worker = executeFixCheckWiki(actionConfig);
    } else if ("FixListCheckWiki".equalsIgnoreCase(action)) {
      worker = executeFixListCheckWiki(actionConfig);
    } else if ("FixFileCheckWiki".equalsIgnoreCase(action)) {
      worker = executeFixFileCheckWiki(actionConfig);
    } else if ("FixLintError".equalsIgnoreCase(action)) {
      worker = executeFixLintError(actionConfig);
    } else if ("MarkCheckWiki".equalsIgnoreCase(action)) {
      worker = executeMarkCheckWiki(actionConfig);
    } else if ("ListCheckWiki".equalsIgnoreCase(action)) {
      worker = executeListCheckWiki(actionConfig);
    } else if ("FixDump".equalsIgnoreCase(action)) {
      worker = executeFixDump(actionConfig);
    } else if ("Set".equalsIgnoreCase(action)) {
      actionDone = executeSet(actionConfig);
    }

    // Execute action
    if (worker != null) {
      log.info("Running task {}", actionConfig.fullAction);
      worker.setListener(this);
      worker.setTimeLimit(timeLimit);
      worker.start();
    } else if (!actions.isEmpty()) {
      if (!actionDone) {
        log.warn("Unknown task {}", actionConfig.fullAction);
      }
      executeAction(actions.remove(0));
    }
  }

  /**
   * Execute an action of type DoTasks.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private boolean executeDoTasks(Action actionConfig) {
    if (actionConfig.actionArgs.length > 0) {
      File tasks = (actionConfig.baseDir != null) ?
          new File(actionConfig.baseDir, actionConfig.actionArgs[0]) :
          new File(actionConfig.actionArgs[0]);
      int actionNum = 0;
      try (BufferedReader reader = new BufferedReader(new FileReader(tasks))) {
        String line = null;
        while ((line = reader.readLine()) != null) {
          if (line.trim().length() > 0) {
            String[] tmpArgs = line.split(" +");
            if ((tmpArgs != null) && (tmpArgs.length > 0)) {
              actions.add(actionNum, new Action(tmpArgs, tasks.getParentFile()));
              actionNum++;
            }
          }
        }
      } catch (IOException e) {
        log.error("Unable to process task {}: {}",
            actionConfig.fullAction, e.getMessage());
      }
    }

    return true;
  }

  /**
   * Execute an action of type UpdateDabWarning.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeUpdateDabWarning(Action actionConfig) {
    Configuration config = Configuration.getConfiguration();
    String start = config.getString(null, ConfigurationValueString.LAST_DAB_WARNING);
    if (actionConfig.actionArgs.length > 0) {
      if (actionConfig.actionArgs[0].equals("*")) {
        start = null;
      } else {
        start = actionConfig.actionArgs[0];
      }
    }
    return new UpdateDabWarningWorker(wiki, null, start);
  }

  /**
   * Execute an action of type FixCheckWiki.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeFixCheckWiki(Action actionConfig) {
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    List<CheckErrorAlgorithm> allAlgorithms = new ArrayList<>();
    if (actionConfig.actionArgs.length > 0) {
      extractAlgorithms(algorithms, allAlgorithms, actionConfig.actionArgs, 0);
    }
    AutomaticCWWorker worker = new AutomaticCWWorker(
        wiki, null, algorithms, 10000, true, allAlgorithms, null, true, false);
    worker.setRange(rangeBegin, rangeEnd);
    return worker;
 }

  /**
   * Execute an action of type FixListCheckWiki.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeFixListCheckWiki(Action actionConfig) {
    Page page = null;
    if (actionConfig.actionArgs.length > 0) {
      page = DataManager.createSimplePage(
          wiki, actionConfig.actionArgs[0], null, null, null);
    }
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    List<CheckErrorAlgorithm> allAlgorithms = new ArrayList<>();
    if (actionConfig.actionArgs.length > 1) {
      extractAlgorithms(algorithms, allAlgorithms, actionConfig.actionArgs, 1);
    }
    AutomaticListCWWorker worker = new AutomaticListCWWorker(
        wiki, null, page,
        algorithms, allAlgorithms, namespaces,
        null, true, false);
    worker.setRange(rangeBegin, rangeEnd);
    return worker;
  }

  /**
   * Execute an action of type FixFileCheckWiki.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeFixFileCheckWiki(Action actionConfig) {
    String path = null;
    if (actionConfig.actionArgs.length > 0) {
      path = actionConfig.actionArgs[0];
    }
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    List<CheckErrorAlgorithm> allAlgorithms = new ArrayList<>();
    if (actionConfig.actionArgs.length > 1) {
      extractAlgorithms(algorithms, allAlgorithms, actionConfig.actionArgs, 1);
    }
    AutomaticFileCWWorker worker = new AutomaticFileCWWorker(
        wiki, null, path,
        algorithms, allAlgorithms, namespaces,
        null, true, false);
    worker.setRange(rangeBegin, rangeEnd);
    return worker;
  }

  /**
   * Execute an action of type FixLintError.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeFixLintError(Action actionConfig) {
    if (actionConfig.actionArgs.length == 0) {
      return null;
    }
    String categoryName = actionConfig.actionArgs[0];
    List<LinterCategory> categories = wiki.getWikiConfiguration().getLinterCategories();
    if (categories == null) {
      return null;
    }
    LinterCategory category = null;
    for (LinterCategory tmpCategory : categories) {
      if (tmpCategory.getCategory().equalsIgnoreCase(categoryName)) {
        category = tmpCategory;
      }
    }
    if (category == null) {
      return null;
    }
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    List<CheckErrorAlgorithm> allAlgorithms = new ArrayList<>();
    if (actionConfig.actionArgs.length > 1) {
      extractAlgorithms(algorithms, allAlgorithms, actionConfig.actionArgs, 1);
    }
    AutomaticLintErrorWorker worker = new AutomaticLintErrorWorker(
        wiki, null, category,
        algorithms, allAlgorithms, namespaces,
        null, true, false);
    worker.setRange(rangeBegin, rangeEnd);
    return worker;
  }

  /**
   * Execute an action of type MarkCheckWiki.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeMarkCheckWiki(Action actionConfig) {
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    List<CheckErrorAlgorithm> allAlgorithms = new ArrayList<>();
    if (actionConfig.actionArgs.length > 0) {
      extractAlgorithms(algorithms, allAlgorithms, actionConfig.actionArgs, 0);
    }
    AutomaticCWWorker worker = new AutomaticCWWorker(
        wiki, null, algorithms, 10000, true, allAlgorithms, null, false, false);
    return worker;
  }

  /**
   * Execute an action of type ListCheckWiki.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeListCheckWiki(Action actionConfig) {

    // Check for global parameters
    String[] actionArgs = actionConfig.actionArgs;
    int currentArg = 0;
    boolean check = true;
    boolean onlyRecheck = false;
    boolean optionsFinished = false;
    while (!optionsFinished && (actionArgs.length > currentArg)) {
      if ("-nocheck".equalsIgnoreCase(actionArgs[currentArg])) {
        check = false;
        currentArg++;
      } else if ("-onlyRecheck".equalsIgnoreCase(actionArgs[currentArg])) {
        onlyRecheck = true;
        currentArg++;
      } else {
        optionsFinished = true;
      }
    }

    // Check for parameters
    if (actionArgs.length > currentArg + 2) {
      File dumpFile = getDumpFile(actionArgs[currentArg]);
      List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
      extractAlgorithms(algorithms, null, actionArgs, currentArg + 2);
      if (actionArgs[currentArg + 1].startsWith("wiki:")) {
        String pageName = actionArgs[currentArg + 1].substring(5);
        return new ListCWWorker(
            wiki, null, dumpFile, pageName,
            algorithms, namespaces, check, onlyRecheck);
      }
      File output = new File(actionArgs[currentArg + 1]);
      return new ListCWWorker(
          wiki, null, dumpFile, output,
          algorithms, namespaces, check);
    }

    return null;
  }

  /**
   * Execute an action of type FixDump.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private BasicWorker executeFixDump(Action actionConfig) {

    // Check for global parameters
    String[] actionArgs = actionConfig.actionArgs;
    int currentArg = 0;

    // Check for parameters
    if (actionArgs.length > currentArg + 1) {
      File dumpFile = getDumpFile(actionArgs[currentArg]);
      List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
      List<CheckErrorAlgorithm> allAlgorithms = new ArrayList<>();
      extractAlgorithms(algorithms, allAlgorithms, actionArgs, currentArg + 1);
      return new FixDumpWorker(
          wiki, null, dumpFile,
          algorithms, allAlgorithms,
          namespaces);
    }

    return null;
  }

  /**
   * Execute an action of type Set.
   * 
   * @param actionConfig Parameters of the action.
   * @return True if the action was executed.
   */
  private boolean executeSet(Action actionConfig) {
    String[] actionArgs = actionConfig.actionArgs;
    if (actionArgs.length < 1) {
      return false;
    }
    String parameter = actionArgs[0];

    // Set AdditionalAlgorithms
    if ("AdditionalAlgorithms".equalsIgnoreCase(parameter)) {
      additionalAlgorithms.clear();
      if (actionArgs.length > 1) {
        extractAlgorithms(additionalAlgorithms, null, actionArgs, 1);
      }
      return true;
    }

    // Set Configuration
    if ("Configuration".equalsIgnoreCase(parameter)) {
      if (actionArgs.length > 2) {
        Configuration config = Configuration.getConfiguration();
        config.forceValue(actionArgs[1], actionArgs[2]);
        for (CheckErrorAlgorithm algorithm : CheckErrorAlgorithms.getAlgorithms(wiki)) {
          algorithm.setConfiguration(wiki.getWikiConfiguration(), wiki.getCWConfiguration(), wiki.getConfiguration());
        }
      }
      return true;
    }

    // Set Namespaces
    if ("Namespaces".equalsIgnoreCase(parameter)) {
      namespaces = new HashSet<>();
      int currentArg = 1;
      while (currentArg < actionArgs.length) {
        try {
          namespaces.add(Integer.valueOf(actionArgs[currentArg]));
        } catch (NumberFormatException e) {
          log.warn("Incorrect namespace {}", actionArgs[currentArg]);
        }
        currentArg++;
      }
      return true;
    }

    // Set Prefix
    if ("Prefix".equalsIgnoreCase(parameter)) {
      if (actionArgs.length > 1) {
        CommentManager.addExtraText(actionArgs[1].replaceAll("_", " "));
      }
      return true;
    }

    // Set Range
    if ("RangeBegin".equalsIgnoreCase(parameter)) {
      rangeBegin = null;
      if (actionArgs.length > 1) {
        rangeBegin = actionArgs[1];
      }
      return true;
    }
    if ("RangeEnd".equalsIgnoreCase(parameter)) {
      rangeEnd = null;
      if (actionArgs.length > 1) {
        rangeEnd = actionArgs[1];
      }
      return true;
    }

    // Set Time limit
    if ("TimeLimit".equalsIgnoreCase(parameter) &&
        (actionArgs.length > 1)) {
      try {
        timeLimit = System.currentTimeMillis() + 1000 * Long.parseLong(actionArgs[1]);
      } catch (NumberFormatException e) {
        log.warn("Incorrect time limit {}", actionArgs[1]);
      }
      return true;
    }
    
    // Set Typo groups
    if ("TypoGroups".equalsIgnoreCase(parameter) &&
        (actionArgs.length > 1)) {
      typoGroups.clear();
      for (int numArg = 1; numArg < actionArgs.length; numArg++) {
        typoGroups.add(actionArgs[numArg]);
      }
      return true;
    }

    return false;
  }

  /**
   * @param algorithms List of selected algorithms.
   * @param allAlgorithms List of all possible algorithms.
   * @param args Arguments.
   * @param startIndex Start index in the arguments.
   */
  private void extractAlgorithms(
      List<CheckErrorAlgorithm> algorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      String[] args, int startIndex) {

    // Create list based on arguments
    for (int i = startIndex; i < args.length; i++) {
      boolean addition = false;
      String algorithmNumber = args[i];
      if (algorithmNumber.startsWith("+")) {
        addition = true;
        algorithmNumber = algorithmNumber.substring(1);
      }
      if (algorithmNumber.equals("*") || algorithmNumber.equals("!")) {
        // NOTE: Allowing "!" because of Eclipse bug with "*"
        // https://bugs.eclipse.org/bugs/show_bug.cgi?id=212264
        List<CheckErrorAlgorithm> possibleAlgorithms = CheckErrorAlgorithms.getAlgorithms(wiki);
        for (CheckErrorAlgorithm algorithm : possibleAlgorithms) {
          if ((algorithm != null) && algorithm.isAvailable()) {
            if (!addition &&
                !algorithms.contains(algorithm)) {
              algorithms.add(algorithm);
            }
            if ((allAlgorithms != null)  &&
                !allAlgorithms.contains(algorithm)) {
              allAlgorithms.add(algorithm);
            }
          }
        }
      } else {
        CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, Integer.parseInt(algorithmNumber));
        if (algorithm != null) {
          if (!addition &&
              !algorithms.contains(algorithm)) {
            algorithms.add(algorithm);
          }
          if ((allAlgorithms != null) &&
              !allAlgorithms.contains(algorithm)) {
            allAlgorithms.add(algorithm);
          }
        }
      }
    }

    // Add additional algorithms
    if (allAlgorithms != null) {
      for (CheckErrorAlgorithm algorithm : additionalAlgorithms) {
        if (!allAlgorithms.contains(algorithm)) {
          allAlgorithms.add(algorithm);
        }
      }
    }
  }

  /**
   * @param path Path to the dump file.
   * @return Dump file.
   */
  private File getDumpFile(String path) {
    File file = new File(path);
    if (file.exists() && file.isFile() && file.canRead()) {
      return file;
    }
    if (!file.isAbsolute()) {
      String dumpsDir = System.getenv("DUMPS_DIR");
      if (dumpsDir != null) {
        file = new File(dumpsDir, path);
      }
    }
    File parent = file.getParentFile();
    if ((parent == null) || (!parent.exists()) || (!parent.isDirectory())) {
      return null;
    }
    final String filename = file.getName();
    final int starIndex = filename.indexOf('$');
    if (starIndex < 0) {
      return null;
    }
    String[] filenames = parent.list(new FilenameFilter() {
      
      @Override
      public boolean accept(@SuppressWarnings("unused") File dir, String name) {
        if (name.startsWith(filename.substring(0, starIndex)) &&
            name.endsWith(filename.substring(starIndex + 1))) {
          return true;
        }
        return false;
      }
    });
    if (filenames.length == 0) {
      return null;
    }
    Arrays.sort(filenames);
    return new File(parent, filenames[filenames.length - 1]);
  }

  /**
   * @param worker Worker.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#beforeStart(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  @Override
  public void beforeStart(BasicWorker worker) {
    // Do nothing
  }

  /**
   * @param worker Worker.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#afterStart(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  @Override
  public void afterStart(BasicWorker worker) {
    // Do nothing
  }

  /**
   * @param worker Worker.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#beforeFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  @Override
  public void beforeFinished(BasicWorker worker) {
    // Do nothing
  }

  /**
   * @param worker Worker.
   * @param ok True if it finished OK.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#afterFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker, boolean)
   */
  @Override
  public void afterFinished(BasicWorker worker, boolean ok) {
    if (!ok) {
      log.error("Task finished in error, exiting");
      System.exit(1);
    }
    if (!loginDone) {
      loginDone = true;
      CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, 501);
      if (algorithm instanceof CheckErrorAlgorithm501) {
        ((CheckErrorAlgorithm501) algorithm).setAuthorizedGroups(typoGroups);
      }
    }
    if (actions.isEmpty()) {
      System.exit(0);
    }
    Action currentAction = actions.remove(0);
    executeAction(currentAction);
  }

  /**
   * Bean for an action.
   */
  private static class Action {

    /** Action itself */
    public final String action;

    /** List of arguments for the action */
    public final String[] actionArgs;

    /** Full action (with its arguments) */
    public final String fullAction;

    /** Base directory */
    public final File baseDir;

    /**
     * Constructor.
     * 
     * @param args List of arguments for the action.
     * @param baseDir Base directory.
     */
    public Action(String[] args, File baseDir) {
      this.action = (args != null) && (args.length > 0) ?
          args[0] : null;
      this.actionArgs = (args != null) && (args.length > 1) ?
          Arrays.copyOfRange(args, 1, args.length) : new String[0];
      StringJoiner fullActionJoiner = new StringJoiner(" ");
      if (args != null) {
        for (String arg : args) {
          fullActionJoiner.add(arg);
        }
      }
      this.fullAction = fullActionJoiner.toString();
      this.baseDir = baseDir;
    }

    /**
     * @return True if the action is OK.
     */
    public boolean isOk() {
      return (action != null);
    }

    /**
     * @return Textual description.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      return fullAction;
    }
  }
}
