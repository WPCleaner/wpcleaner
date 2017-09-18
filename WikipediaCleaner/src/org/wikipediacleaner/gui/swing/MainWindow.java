/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.beans.EventHandler;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumQueryPage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.AbuseFilter;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.Suggestion;
import org.wikipediacleaner.gui.swing.action.ActionDisambiguationAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionUpdateWarning;
import org.wikipediacleaner.gui.swing.action.ActionUtilities;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.BasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.HTMLPane;
import org.wikipediacleaner.gui.swing.pagelist.PageListWorker;
import org.wikipediacleaner.gui.swing.worker.LoginWorker;
import org.wikipediacleaner.gui.swing.worker.RandomPageWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;
import org.wikipediacleaner.utils.ConfigurationValueString;
import org.wikipediacleaner.utils.StringChecker;

/**
 * Main Window of WikipediaCleaner. 
 */
public class MainWindow
  extends BasicWindow
  implements ActionListener {

  public final static Integer WINDOW_VERSION = Integer.valueOf(6);

  private final static String URL_OTHER_LANGUAGE  = "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner/Languages";
  private final static String URL_OTHER_WIKIPEDIA = "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner/Wikis";
  private final static String URL_TALK_PAGE       = "http://fr.wikipedia.org/wiki/Discussion_Wikipédia:WPCleaner";

  JComboBox<EnumWikipedia> comboWikipedia;
  private JComboBox<EnumLanguage> comboLanguage;
  JComboBox<Object> comboUser;
  JPasswordField textPassword;
  private char echoPassword = '*';
  private ButtonGroup groupSaveUsernamePassword;
  private JRadioButton radSavePassword;
  private JRadioButton radSaveUsername;
  private JRadioButton radSaveNothing;
  private JButton buttonLogin;
  private JButton buttonDemo;
  private JButton buttonLogout;
  private JButton buttonDisconnect;
  private JButton buttonHelp;

  private JButton buttonAbuseFilters;
  private JButton buttonAllDab;
  private JButton buttonBotTools;
  private JButton buttonCheckWiki;
  private JButton buttonCurrentDabList;
  private JButton buttonGenerateLists;
  private JButton buttonHelpRequested;
  private JButton buttonMostDabLinks;
  private JButton buttonRandomPages;
  private JButton buttonSpecialLists;
  private JButton buttonLinterCategories;
  private JButton buttonWatchlistLocal;
  private JButton buttonWatchlist;

  private JComboBox<String> comboPagename;
  private JButton buttonFullAnalysis;
  private JButton buttonDisambiguation;
  private JButton buttonSearchTitles;
  private JButton buttonBackLinks;
  private JButton buttonEmbeddedIn;
  private JButton buttonInternalLinks;
  private JButton buttonCategoryMembers;
  private JButton buttonUpdateWarning;
  private JButton buttonAddPage;
  private JButton buttonRemovePage;
  private JButton buttonRandomPage;
  private JButton buttonLoadList;
  private JButton buttonContributions;

  private JButton buttonOptions;
  private JButton buttonOptionsSystem;
  private JButton buttonReloadOptions;
  private JButton buttonCheckSpelling;
  private JButton buttonIdea;
  private JButton buttonAbout;

  boolean logged = false;
  boolean userLogged = false;

  private static class MainWindowListener implements BasicWindowListener {

    private final EnumWikipedia wiki;
    private final String userName;
    private final String password;

    public MainWindowListener(EnumWikipedia wiki, String userName, String password) {
      this.wiki = wiki;
      this.userName = userName;
      this.password = password;
    }

    /**
     * Called just after BasicWindow constructor has been called.
     * 
     * @param window BasicWindow.
     * @see org.wikipediacleaner.gui.swing.basic.BasicWindowListener#initializeWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
     */
    @Override
    public void initializeWindow(BasicWindow window) {
      // Nothing to do
    }

    /**
     * Called just after BasicWindow has been displayed.
     * 
     * @param window BasicWindow.
     * @see org.wikipediacleaner.gui.swing.basic.BasicWindowListener#displayWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
     */
    @Override
    public void displayWindow(BasicWindow window) {
      Configuration config = Configuration.getConfiguration();
      config.checkVersion(window.getParentComponent());

      MainWindow mainWindow = (MainWindow) window;
      if (wiki != null) {
        mainWindow.comboWikipedia.setSelectedItem(wiki);
      }
      if (userName != null) {
        mainWindow.comboUser.setSelectedItem(userName);
      }
      if (password != null) {
        mainWindow.textPassword.setText(password);
      }
      if ((wiki != null) && (userName != null) && (password != null)) {
        mainWindow.actionLogin();
      }
    }
    
  }

  /**
   * Create and display a MainWindow.
   */
  public static void createMainWindow(EnumWikipedia wiki, String userName, String password) {
    createWindow(
        "MainWindow",
        null,
        JFrame.EXIT_ON_CLOSE,
        MainWindow.class,
        new MainWindowListener(wiki, userName, password));
  }

  /**
   * @return Window title.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._(
        "{0} - Version {1} ({2})",
        new Object[] { Version.PROGRAM, Version.VERSION, DateFormat.getDateInstance().format(Version.DATE) });
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;

    // Main components
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weighty = 1;
    panel.add(createLoginComponents(), constraints);
    constraints.gridx++;
    panel.add(createProjectsComponents(), constraints);
    constraints.gridx++;
    panel.add(createPageComponents(), constraints);
    constraints.gridy++;

    // Message components
    if ((Version.MESSAGE != null) && !Version.MESSAGE.equals("")) {
      try {
        Component messageComponents = createMessageComponents();
        constraints.gridwidth = 3;
        constraints.gridx = 0;
        constraints.weighty = 0;
        panel.add(messageComponents, constraints);
        constraints.gridy++;
      } catch (Throwable t) {
        logError("Unable to create message components", t);
      }
    }

    updateComponentState();
    return panel;
  }

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    comboWikipedia.setEnabled(!logged);
    comboLanguage.setEnabled(!logged);
    comboUser.setEnabled(!logged);
    textPassword.setEnabled(!logged);
    textPassword.setEchoChar(logged ? ' ' : echoPassword);
    buttonLogin.setEnabled(!logged);
    buttonDemo.setEnabled(!logged);
    buttonLogout.setEnabled(logged);
    buttonDisconnect.setEnabled(logged);
    buttonOptionsSystem.setEnabled(logged);
    buttonReloadOptions.setEnabled(logged);
    buttonCheckSpelling.setEnabled(logged);

    buttonAbuseFilters.setEnabled(logged);
    buttonAllDab.setEnabled(logged);
    buttonBotTools.setEnabled(userLogged);
    buttonCheckWiki.setEnabled(logged);
    buttonCurrentDabList.setEnabled(logged);
    buttonGenerateLists.setEnabled(logged);
    buttonHelpRequested.setEnabled(logged);
    buttonMostDabLinks.setEnabled(logged);
    buttonRandomPages.setEnabled(logged);
    buttonSpecialLists.setEnabled(logged);
    buttonLinterCategories.setEnabled(logged);
    buttonWatchlistLocal.setEnabled(logged);
    buttonWatchlist.setEnabled(logged);

    comboPagename.setEnabled(logged);
    buttonFullAnalysis.setEnabled(logged);
    buttonDisambiguation.setEnabled(logged);
    buttonSearchTitles.setEnabled(logged);
    buttonInternalLinks.setEnabled(logged);
    buttonBackLinks.setEnabled(logged);
    buttonCategoryMembers.setEnabled(logged);
    buttonEmbeddedIn.setEnabled(logged);
    buttonUpdateWarning.setEnabled(logged);
    buttonRandomPage.setEnabled(logged);
    buttonAddPage.setEnabled(logged);
    buttonRemovePage.setEnabled(logged);
    buttonLoadList.setEnabled(logged);
    buttonContributions.setEnabled(logged);
  }

  /**
   * @return Message components.
   */
  private Component createMessageComponents() {
    JPanel panel = new JPanel(new GridLayout(1, 0));
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(
            Version.HIGHLIGHT ? Color.RED : null,
            Version.HIGHLIGHT ? Color.RED : null),
        GT._("Message"),
        TitledBorder.CENTER, TitledBorder.DEFAULT_POSITION));
    if (Version.HIGHLIGHT) {
      panel.setBackground(Color.RED);
    }
    HTMLPane textMessage = HTMLPane.createHTMLPane(Version.MESSAGE);
    JScrollPane scrollMessage = new JScrollPane(textMessage);
    scrollMessage.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollMessage);
    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createLoginComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Login")));
    Configuration configuration = Configuration.getConfiguration();

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(1, 1, 1, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Wikipedia
    EnumWikipedia defaultWikipedia = configuration.getWikipedia();
    comboWikipedia = new JComboBox<EnumWikipedia>(EnumWikipedia.getList().toArray(new EnumWikipedia[0]));
    comboWikipedia.setEditable(false);
    comboWikipedia.setSelectedItem(defaultWikipedia);
    comboWikipedia.addItemListener(EventHandler.create(
        ItemListener.class, this, "actionChangeWiki"));
    JLabel labelWikipedia = Utilities.createJLabel(GT._("Wiki"));
    labelWikipedia.setLabelFor(comboWikipedia);
    labelWikipedia.setHorizontalAlignment(SwingConstants.TRAILING);
    JToolBar toolbarWikipedia = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarWikipedia.setFloatable(false);
    toolbarWikipedia.setBorderPainted(false);
    JButton buttonWikipediaInfo = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.SMALL,
        GT._("Other Wikipedia"), false, null);
    buttonWikipediaInfo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOtherWikipedia"));
    toolbarWikipedia.add(buttonWikipediaInfo);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelWikipedia, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(comboWikipedia, constraints);
    constraints.gridx = 2;
    constraints.weightx = 0;
    panel.add(toolbarWikipedia, constraints);
    constraints.gridy++;

    // Language
    comboLanguage = new JComboBox<EnumLanguage>(EnumLanguage.getList().toArray(new EnumLanguage[0]));
    comboLanguage.setEditable(false);
    comboLanguage.setSelectedItem(configuration.getLanguage());
    comboLanguage.addItemListener(EventHandler.create(
        ItemListener.class, this, "actionChangeLanguage"));
    JLabel labelLanguage = Utilities.createJLabel(GT._("Language"));
    labelLanguage.setLabelFor(comboLanguage);
    labelLanguage.setHorizontalAlignment(SwingConstants.TRAILING);
    JToolBar toolbarLanguage = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarLanguage.setFloatable(false);
    toolbarLanguage.setBorderPainted(false);
    JButton buttonLanguageInfo = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.SMALL,
        GT._("Other Language"), false, null);
    buttonLanguageInfo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOtherLanguage"));
    toolbarLanguage.add(buttonLanguageInfo);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelLanguage, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(comboLanguage, constraints);
    constraints.gridx = 2;
    constraints.weightx = 0;
    panel.add(toolbarLanguage, constraints);
    constraints.gridy++;

    // User name
    comboUser = new JComboBox<Object>();
    comboUser.setEditable(true);
    comboUser.addItemListener(EventHandler.create(
        ItemListener.class, this, "actionChangeUser"));
    JLabel labelUsername = Utilities.createJLabel(GT._("Username:"));
    labelUsername.setLabelFor(comboUser);
    labelUsername.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelUsername, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(comboUser, constraints);
    constraints.gridy++;

    // Password
    textPassword = new JPasswordField();
    textPassword.setText("");
    echoPassword = textPassword.getEchoChar();
    JLabel labelPassword = Utilities.createJLabel(GT._("Password :"));
    labelPassword.setLabelFor(textPassword);
    labelPassword.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelPassword, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(textPassword, constraints);
    constraints.gridy++;

    // Login/Demo/Logout buttons
    JToolBar buttonToolbar = new JToolBar(SwingConstants.HORIZONTAL);
    buttonToolbar.setFloatable(false);
    buttonToolbar.setBorderPainted(false);
    buttonLogin = Utilities.createJButton(
        GT._("Login"),
        ConfigurationValueShortcut.LOGIN);
    buttonLogin.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLogin"));
    buttonToolbar.add(buttonLogin);
    buttonToolbar.addSeparator();
    buttonDemo = Utilities.createJButton(GT._("Demo"), null);
    buttonDemo.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDemo"));
    buttonToolbar.add(buttonDemo);
    buttonToolbar.addSeparator();
    buttonLogout = Utilities.createJButton(
        GT._("Wiki logout"),
        ConfigurationValueShortcut.LOGOUT);
    buttonLogout.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLogout"));
    buttonToolbar.add(buttonLogout);
    buttonToolbar.addSeparator();
    buttonDisconnect = Utilities.createJButton(
        GT._("WPC logout"), null);
    buttonDisconnect.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisconnect"));
    buttonToolbar.add(buttonDisconnect);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    panel.add(buttonToolbar, constraints);
    constraints.gridy++;

    // Buttons
    buttonToolbar = new JToolBar(SwingConstants.HORIZONTAL);
    buttonToolbar.setFloatable(false);
    buttonToolbar.setBorderPainted(false);
    buttonHelp = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.NORMAL,
        GT._("Help"), false,
        ConfigurationValueShortcut.HELP);
    buttonHelp.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionHelp"));
    buttonToolbar.add(buttonHelp);
    buttonOptions = Utilities.createJButton(
        "gnome-preferences-other.png", EnumImageSize.NORMAL,
        GT._("Options"), false,
        ConfigurationValueShortcut.OPTIONS);
    buttonOptions.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOptions"));
    buttonToolbar.add(buttonOptions);
    buttonOptionsSystem = Utilities.createJButton(
        "gnome-preferences-system.png", EnumImageSize.NORMAL,
        GT._("System options"), false,
        ConfigurationValueShortcut.SYSTEM_OPTIONS);
    buttonOptionsSystem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOptionsSystem"));
    buttonToolbar.add(buttonOptionsSystem);
    buttonReloadOptions = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        GT._("Reload system options"), false, null);
    buttonReloadOptions.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionReloadOptions"));
    buttonToolbar.add(buttonReloadOptions);
    buttonCheckSpelling = Utilities.createJButton(
        "gnome-tools-check-spelling.png", EnumImageSize.NORMAL,
        GT._("Check spelling options"), false, null);
    buttonCheckSpelling.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCheckSpelling"));
    buttonToolbar.add(buttonCheckSpelling);
    buttonToolbar.addSeparator();
    buttonIdea = Utilities.createJButton(
        GT._("Idea? Bug?"),
        ConfigurationValueShortcut.BUG_REPORT);
    buttonIdea.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionIdea"));
    buttonToolbar.add(buttonIdea);
    buttonToolbar.addSeparator();
    buttonAbout = Utilities.createJButton(GT._("About"), null);
    buttonAbout.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAbout"));
    buttonToolbar.add(buttonAbout);
    constraints.fill = GridBagConstraints.NONE;
    constraints.weighty = 0;
    panel.add(buttonToolbar, constraints);
    constraints.gridy++;

    // Save password
    int saveUser = configuration.getInt(
        null, ConfigurationValueInteger.SAVE_USER);
    groupSaveUsernamePassword = new ButtonGroup();
    radSavePassword = Utilities.createJRadioButton(
        GT._("Save username and password"),
        (saveUser == ConfigurationConstants.VALUE_SAVE_USER_BOTH));
    radSavePassword.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSavePassword"));
    groupSaveUsernamePassword.add(radSavePassword);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.gridwidth = 2;
    panel.add(radSavePassword, constraints);
    constraints.gridy++;
    radSaveUsername = Utilities.createJRadioButton(
        GT._("Save username only"),
        (saveUser == ConfigurationConstants.VALUE_SAVE_USER_NAME));
    groupSaveUsernamePassword.add(radSaveUsername);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.gridwidth = 2;
    panel.add(radSaveUsername, constraints);
    constraints.gridy++;
    radSaveNothing = Utilities.createJRadioButton(
        GT._("Save none of them"),
        (saveUser != ConfigurationConstants.VALUE_SAVE_USER_BOTH) &&
        (saveUser != ConfigurationConstants.VALUE_SAVE_USER_NAME));
    groupSaveUsernamePassword.add(radSaveNothing);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.gridwidth = 2;
    panel.add(radSaveNothing, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);
    constraints.gridy++;

    actionChangeWiki();

    return panel;
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Page")));
    Configuration configuration = Configuration.getConfiguration();

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 1, 0, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Page name
    String lastPage = "";
    if (configuration.getBoolean(
        null,
        ConfigurationValueBoolean.REMEMBER_LAST_PAGE)) {
      lastPage = configuration.getString(null, ConfigurationValueString.PAGE_NAME);
    }
    List<String> interestingPages = configuration.getStringList(
        null, Configuration.ARRAY_INTERESTING_PAGES);
    if (interestingPages != null) {
      comboPagename = new JComboBox<String>(interestingPages.toArray(new String[0]));
    } else {
      comboPagename = new JComboBox<String>();
    }
    comboPagename.setEditable(true);
    comboPagename.setPrototypeDisplayValue("XXXXXXXXXXXXXXXXXXXXXXXXX");
    comboPagename.setSelectedItem(lastPage);
    panel.add(comboPagename, constraints);
    constraints.gridx++;

    // Random page button
    JToolBar toolbarPage = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarPage.setFloatable(false);
    toolbarPage.setBorderPainted(false);
    buttonRandomPage = Utilities.createJButton(
        "commons-nuvola-apps-atlantik.png", EnumImageSize.SMALL,
        GT._("Random page"), false,
        ConfigurationValueShortcut.RANDOM_PAGE);
    buttonRandomPage.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRandomPage"));
    toolbarPage.add(buttonRandomPage);
    buttonAddPage = Utilities.createJButton(
        "gnome-list-add.png", EnumImageSize.SMALL,
        GT._("Add to list"), false,
        ConfigurationValueShortcut.LIST_ADD);
    buttonAddPage.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAddPage"));
    toolbarPage.add(buttonAddPage);
    buttonRemovePage = Utilities.createJButton(
        "gnome-list-remove.png", EnumImageSize.SMALL,
        GT._("Remove from list"), false,
        ConfigurationValueShortcut.LIST_REMOVE);
    buttonRemovePage.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRemovePage"));
    toolbarPage.add(buttonRemovePage);
    constraints.weightx = 0;
    panel.add(toolbarPage, constraints);
    constraints.gridy++;

    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;

    // Full analysis button
    buttonFullAnalysis = ActionFullAnalysis.createButton(getWikipedia(), null, true, true, true);
    panel.add(buttonFullAnalysis, constraints);
    constraints.gridy++;

    // Disambiguation button
    buttonDisambiguation = ActionDisambiguationAnalysis.createButton(getWikipedia(), null, true, true, true);
    panel.add(buttonDisambiguation, constraints);
    constraints.gridy++;

    // Search button
    buttonSearchTitles = Utilities.createJButton(
        "gnome-system-search.png", EnumImageSize.NORMAL,
        GT._("Search in titles"), true, null);
    buttonSearchTitles.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSearchTitles"));
    panel.add(buttonSearchTitles, constraints);
    constraints.gridy++;

    // Links button
    buttonInternalLinks = Utilities.createJButton(
        "wpc-internal-link.png", EnumImageSize.NORMAL,
        GT._("Internal links"), true, null);
    buttonInternalLinks.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionInternalLinks"));
    panel.add(buttonInternalLinks, constraints);
    constraints.gridy++;

    // Backlinks
    buttonBackLinks = Utilities.createJButton(
        "wpc-internal-link.png", EnumImageSize.NORMAL,
        GT._("What links here"), true, null);
    buttonBackLinks.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionBackLinks"));
    panel.add(buttonBackLinks, constraints);
    constraints.gridy++;

    // Category members
    buttonCategoryMembers = Utilities.createJButton(
        "commons-nuvola-apps-kpager.png", EnumImageSize.NORMAL,
        GT._("Category members"), true, null);
    buttonCategoryMembers.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCategoryMembers"));
    panel.add(buttonCategoryMembers, constraints);
    constraints.gridy++;

    // Embedded in
    buttonEmbeddedIn = Utilities.createJButton(
        "commons-curly-brackets.png", EnumImageSize.NORMAL,
        GT._("Embedded in"), true, null);
    buttonEmbeddedIn.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionEmbeddedIn"));
    panel.add(buttonEmbeddedIn, constraints);
    constraints.gridy++;

    // Update warnings
    buttonUpdateWarning = ActionUpdateWarning.createButton(
        getParentComponent(), this, this, true, true);
    panel.add(buttonUpdateWarning, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);
    constraints.gridy++;
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;

    // Load list from disk
    buttonLoadList = Utilities.createJButton(
        "gnome-drive-harddisk.png", EnumImageSize.NORMAL,
        GT._("Load list from drive"), true, null);
    buttonLoadList.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLoadList"));
    panel.add(buttonLoadList, constraints);
    constraints.gridy++;

    // Contributions
    buttonContributions = Utilities.createJButton(
        "gnome-utilities-system-monitor.png", EnumImageSize.NORMAL,
        GT._("Your contributions"), true, null);
    buttonContributions.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionContributions"));
    panel.add(buttonContributions, constraints);
    constraints.gridy++;
    return panel;
  }

  /**
   * @return Projects components.
   */
  private Component createProjectsComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Projects")));

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 1, 0, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // All disambiguation pages button
    buttonAllDab = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Preload disambiguations pages"), true, null);
    buttonAllDab.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAllDab"));
    panel.add(buttonAllDab, constraints);
    constraints.gridy++;

    // Current disambiguation list button
    buttonCurrentDabList = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Current disambiguation list"), true,
        ConfigurationValueShortcut.CURRENT_DAB_LIST);
    buttonCurrentDabList.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCurrentDabList"));
    panel.add(buttonCurrentDabList, constraints);
    constraints.gridy++;

    // Pages with most disambiguation links button
    buttonMostDabLinks = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("With many disambiguation links"), true, null);
    buttonMostDabLinks.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMostDabLinks"));
    panel.add(buttonMostDabLinks, constraints);
    constraints.gridy++;

    // Check Wiki Project button
    buttonCheckWiki = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Project Check Wikipedia"), true, null);
    buttonCheckWiki.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCheckWiki"));
    panel.add(buttonCheckWiki, constraints);
    constraints.gridy++;

    // Help requested button
    buttonHelpRequested = Utilities.createJButton(
        "gnome-dialog-question.png", EnumImageSize.NORMAL,
        GT._("Help requested on..."), true, null);
    buttonHelpRequested.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionHelpRequestedOn"));
    panel.add(buttonHelpRequested, constraints);
    constraints.gridy++;

    // Abuse filters
    buttonAbuseFilters = Utilities.createJButton(
        null, EnumImageSize.NORMAL,
        GT._("Abuse filters"), true, null);
    buttonAbuseFilters.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAbuseFilters"));
    panel.add(buttonAbuseFilters, constraints);
    constraints.gridy++;

    // Special lists
    buttonSpecialLists = Utilities.createJButton(
        "gnome-colors-applications-office.png", EnumImageSize.NORMAL,
        GT._("Special lists"), true, null);
    buttonSpecialLists.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSpecialLists"));
    panel.add(buttonSpecialLists, constraints);
    constraints.gridy++;

    // Linter categories
    buttonLinterCategories = Utilities.createJButton(
        "Linter_logo_v2.png", EnumImageSize.NORMAL,
        GT._("Linter categories"), true, null);
    buttonLinterCategories.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLinterCategories"));
    panel.add(buttonLinterCategories, constraints);
    constraints.gridy++;

    // Generate lists
    buttonGenerateLists = Utilities.createJButton(
        "gnome-colors-applications-office.png", EnumImageSize.NORMAL,
        GT._("Generate lists"), true, null);
    buttonGenerateLists.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionGenerateLists"));
    panel.add(buttonGenerateLists, constraints);
    constraints.gridy++;

    // Local watch list button
    buttonWatchlistLocal = Utilities.createJButton(
        "gnome-logviewer.png", EnumImageSize.NORMAL,
        GT._("Local Watchlist"), true,
        ConfigurationValueShortcut.WATCH_LIST);
    buttonWatchlistLocal.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionWatchlistLocal"));
    panel.add(buttonWatchlistLocal, constraints);
    constraints.gridy++;

    // Watch list button
    buttonWatchlist = Utilities.createJButton(
        "gnome-logviewer.png", EnumImageSize.NORMAL,
        GT._("Watchlist"), true, null);
    buttonWatchlist.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionWatchlist"));
    panel.add(buttonWatchlist, constraints);
    constraints.gridy++;

    // Random pages button
    buttonRandomPages = Utilities.createJButton(
        "commons-nuvola-apps-atlantik.png", EnumImageSize.NORMAL,
        GT._("Random pages"), true, null);
    buttonRandomPages.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRandom"));
    panel.add(buttonRandomPages, constraints);
    constraints.gridy++;

    // Bot tools button
    buttonBotTools = Utilities.createJButton(
        "commons-nuvola-apps-kcmsystem.png", EnumImageSize.NORMAL,
        GT._("Bot tools"), true, null);
    buttonBotTools.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionBotTools"));
    panel.add(buttonBotTools, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Action called when About button is pressed.
   */
  public void actionAbout() {
    Controller.runAbout();
  }

  /**
   * Action called when Options button is pressed.
   */
  public void actionOptions() {
    Controller.runOptions();
  }

  /**
   * Action called when Login button is pressed.
   */
  public void actionLogin() {
    actionLoginDemo(true);
  }
  
  /**
   * Action called when Demo button is pressed. 
   */
  public void actionDemo() {
    int answer = displayYesNoWarning(GT._(
        "Demo mode is only available for testing WPCleaner.\n" +
        "You won't be able to modify pages on Wikipedia in Demo mode.\n" +
        "Do you want to continue ?"));
    if (answer == JOptionPane.YES_OPTION) {
      actionLoginDemo(false);
    }
  }

  /**
   * Action called when Other Wikipedia button is pressed.
   */
  public void actionOtherWikipedia() {
    String url = URL_OTHER_WIKIPEDIA;
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      displayUrlMessage(
          GT._("You can learn how to add other Wikipedia at the following URL:"),
          url);
    }
  }

  /**
   * Action called when Other Language button is pressed. 
   */
  public void actionOtherLanguage() {
    String url = URL_OTHER_LANGUAGE;
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      displayUrlMessage(
          GT._("You can learn how to add other languages at the following URL:"),
          url);
    }
  }

  /**
   * Action called when Login or Demo button is pressed.
   * 
   * @param login Flag indicating if login is required.
   */
  private void actionLoginDemo(final boolean login) {

    // Check that correct values are entered in Wikipedia combo
    if ((comboWikipedia == null) || (comboWikipedia.getSelectedIndex() == -1)) {
      displayWarning(
          GT._("You must select which Wikipedia you want to work on before login"),
          comboWikipedia);
      return;
    }
    setWikipedia((EnumWikipedia) comboWikipedia.getSelectedItem());

    // Check that correct values are entered in Language combo
    if ((comboLanguage == null) || (comboLanguage.getSelectedIndex() == -1)) {
      displayWarning(
          GT._("You must select a language before login"),
          comboLanguage);
      return;
    }
    EnumLanguage language = (EnumLanguage) comboLanguage.getSelectedItem();
    GT.setCurrentLanguage(language);

    // Check that correct values are entered for user name
    if (login) {
      if ((comboUser == null) ||
          (comboUser.getSelectedItem() == null) ||
          ("".equals(comboUser.getSelectedItem().toString().trim()))) {
        displayWarning(
            GT._("You must input your username before login"),
            comboUser);
        return;
      }
    }

    // Check that correct values are entered for password
    if (login) {
      if ((textPassword == null) ||
          (textPassword.getPassword() == null) ||
          (textPassword.getPassword().length == 0)) {
          displayWarning(
              GT._("You must input your password before login"),
              textPassword);
          return;       
      }

      // If password is Ok... continue with validation
      char[] password = textPassword.getPassword();
      for (int i = 0; i < password.length; i++) {
        password[i] = '\0';
      }
    }

    // Update actions
    ActionUtilities.removeActionListeners(buttonDisambiguation);
    ActionUtilities.removeActionListeners(buttonFullAnalysis);
    buttonDisambiguation.addActionListener(new ActionDisambiguationAnalysis(
        getParentComponent(), getWikipedia(), comboPagename));
    buttonFullAnalysis.addActionListener(new ActionFullAnalysis(
        getParentComponent(), getWikipedia(), comboPagename));

    // Login
    LoginWorker loginWorker = new LoginWorker(
        getWikipedia(), this, comboPagename,
        (EnumLanguage) comboLanguage.getSelectedItem(),
        comboUser.getSelectedItem().toString(),
        textPassword.getPassword(),
        radSavePassword.isSelected() ?
            ConfigurationConstants.VALUE_SAVE_USER_BOTH :
            radSaveUsername.isSelected() ?
                ConfigurationConstants.VALUE_SAVE_USER_NAME :
                ConfigurationConstants.VALUE_SAVE_USER_NONE,
        login, false);
    loginWorker.setListener(new BasicWorkerListener() {

      /**
       * Called just at the beginning of the start() method in BasicWorker.
       * 
       * @param worker Current worker.
       */
      @Override
      public void beforeStart(BasicWorker worker) {
        // Nothing to do
      }

      /**
       * Called just at the end of the start() method in BasicWorker.
       * 
       * @param worker Current worker.
       */
      @Override
      public void afterStart(BasicWorker worker) {
        // Nothing to do
      }

      /**
       * Called just at the beginning of the finished() method in BasicWorker.
       * 
       * @param worker Current worker.
       */
      @Override
      public void beforeFinished(BasicWorker worker) {
        if (worker instanceof LoginWorker) {
          logged = ((LoginWorker) worker).isLogged();
          if (logged) {
            userLogged = login;
          }
        }
      }

      /**
       * Called just at the end of the finished() method in BasicWorker.
       * 
       * @param worker Current worker.
       * @param ok Flag indicating if the worker finished OK.
       */
      @Override
      public void afterFinished(BasicWorker worker, boolean ok) {
        // Nothing to do
      }
    });
    loginWorker.start();
  }

  /**
   * Action called when Logout button is pressed.
   */
  public void actionLogout() {
    API api = APIFactory.getAPI();
    api.logout(getWikipedia());
    logged = false;
    userLogged = false;
    updateComponentState();
  }

  /**
   * Action called when Disconnect button is pressed.
   */
  public void actionDisconnect() {
    logged = false;
    userLogged = false;
    updateComponentState();
  }

  /**
   * Action called when System Options button is pressed.
   */
  public void actionOptionsSystem() {
    if (Utilities.isDesktopSupported()) {
      EnumWikipedia wikipedia = getWikipedia();
      Utilities.browseURL(wikipedia, wikipedia.getConfigurationPage(), true);
    } else {
      displayUrlMessage(
          GT._("You can learn how to configure {0} at the following URL:", Version.PROGRAM),
          URL_OTHER_WIKIPEDIA);
    }
  }

  /**
   * Action called when Reload System Options button is pressed.
   */
  public void actionReloadOptions() {
    new LoginWorker(
        getWikipedia(), this, comboPagename,
        (EnumLanguage) comboLanguage.getSelectedItem(),
        comboUser.getSelectedItem().toString(),
        textPassword.getPassword(),
        radSavePassword.isSelected() ?
            ConfigurationConstants.VALUE_SAVE_USER_BOTH :
            radSaveUsername.isSelected() ?
                ConfigurationConstants.VALUE_SAVE_USER_NAME :
                ConfigurationConstants.VALUE_SAVE_USER_NONE,
        false, true).start();
  }

  /**
   * Action called when Check Spelling button is pressed.
   */
  public void actionCheckSpelling() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }

    // Retrieve all suggestions grouped by page and chapter
    Map<String, Suggestion> suggestions = wikipedia.getConfiguration().getSuggestions();
    Map<String, List<String>> chapters = Suggestion.getChapters(suggestions.values());
    if (chapters.isEmpty()) {
      return;
    }

    // Construct list of pages containing suggestions
    List<String> pages = new ArrayList<String>();
    pages.addAll(chapters.keySet());
    Collections.sort(pages);

    // Create menu for suggestions
    JPopupMenu menu = new JPopupMenu();
    for (String page : pages) {
      List<String> pageChapters = chapters.get(page);
      if (pageChapters.size() > 1) {
        JMenu pageMenu = new JMenu(page);
        JMenuItem item = new JMenuItem(GT._("Activate all"));
        item.setActionCommand(page);
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionCheckSpellingActivatePage", "actionCommand"));
        pageMenu.add(item);
        item = new JMenuItem(GT._("Deactivate all"));
        item.setActionCommand(page);
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionCheckSpellingDeactivatePage", "actionCommand"));
        pageMenu.add(item);
        pageMenu.addSeparator();
        for (String chapter : pageChapters) {
          boolean active = Suggestion.isChapterActive(page, chapter);
          JMenuItem chapterItem = new JCheckBoxMenuItem(chapter);
          chapterItem.setSelected(active);
          chapterItem.setActionCommand(page + "#" + chapter);
          chapterItem.addActionListener(EventHandler.create(
              ActionListener.class, this,
              active ? "actionCheckSpellingDeactivateChapter" : "actionCheckSpellingActivateChapter",
              "actionCommand"));
          pageMenu.add(chapterItem);
        }
        menu.add(pageMenu);
      } else {
        boolean active = Suggestion.isChapterActive(page, pageChapters.get(0));
        JMenuItem pageItem = new JCheckBoxMenuItem(page);
        pageItem.setSelected(active);
        pageItem.setActionCommand(page + "#" + pageChapters.get(0));
        pageItem.addActionListener(EventHandler.create(
            ActionListener.class, this,
            active ? "actionCheckSpellingDeactivateChapter" : "actionCheckSpellingActivateChapter",
                "actionCommand"));
        menu.add(pageItem);
      }
    }
    menu.show(
        buttonCheckSpelling,
        0,
        buttonCheckSpelling.getHeight());
  }

  /**
   * Action called to activate spell checking suggestions of a page.
   * 
   * @param page Page.
   */
  public void actionCheckSpellingActivatePage(String page) {
    actionCheckSpellingPage(page, true);
  }

  /**
   * Action called to deactivate spell checking suggestions of a page.
   * 
   * @param page Page.
   */
  public void actionCheckSpellingDeactivatePage(String page) {
    actionCheckSpellingPage(page, false);
  }

  /**
   * Action called to activate/deactivate spell checking suggestions of a page.
   * 
   * @param page Page.
   * @param activate True to activate spell checking.
   */
  private void actionCheckSpellingPage(String page, boolean activate) {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    Map<String, Suggestion> suggestions = wikipedia.getConfiguration().getSuggestions();
    Map<String, List<String>> chapters = Suggestion.getChapters(suggestions.values());
    Suggestion.activateChapters(page, chapters.get(page), activate);
  }

  /**
   * Action called to activate spell checking suggestions of a chapter.
   * 
   * @param chapter Chapter.
   */
  public void actionCheckSpellingActivateChapter(String chapter) {
    Suggestion.activateChapters(null, Collections.singletonList(chapter), true);
  }

  /**
   * Action called to deactivate spell checking suggestions of a chapter.
   * 
   * @param chapter Chapter.
   */
  public void actionCheckSpellingDeactivateChapter(String chapter) {
    Suggestion.activateChapters(null, Collections.singletonList(chapter), false);
  }

  /**
   * Action called when Help button is pressed.
   */
  public void actionHelp() {
    EnumWikipedia wikipedia = getWikipedia();
    WPCConfigurationString attributeHelpURL = WPCConfigurationString.HELP_URL;
    String url = EnumWikipedia.EN.getConfiguration().getString(attributeHelpURL);
    if ((wikipedia != null) && (wikipedia.getConfiguration().getString(attributeHelpURL) != null)) {
      url = wikipedia.getConfiguration().getString(attributeHelpURL);
    }
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      displayUrlMessage(
          GT._("You can read the help on {0} at the following URL:", Version.PROGRAM),
          url);
    }
  }

  /**
   * Action called when Idea button is pressed.
   */
  public void actionIdea() {
    String url = URL_TALK_PAGE;
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      displayUrlMessage(
          GT._("You can submit bug reports or feature requests at the following URL:"),
          url);
    }
  }

  /**
   * Action called when Contributions button is pressed.
   */
  public void actionContributions() {
    EnumWikipedia wikipedia = getWikipedia();
    if ((wikipedia != null) && (wikipedia.getContributions() != null)) {
      InformationWindow.createInformationWindow(
          GT._("Your contributions"),
          wikipedia.getContributions().getDescription(), true,
          wikipedia);
    }
  }
  
  /**
   * Action called when Save Password is changed. 
   */
  public void actionSavePassword() {
    if ((radSavePassword.isSelected())) {
      int answer = displayYesNoWarning(
          GT._("The password will be saved on your disk, " +
               "so anyone having access to your computer may be able to get it.\n" +
               "Are you sure that you want to save it ?"));
      if (answer != JOptionPane.YES_OPTION) {
        radSaveUsername.setSelected(true);
      }
    }
  }

  /**
   * Check that page name is given.
   * 
   * @param message Message to display if it's not given.
   * @return Page name if it is given.
   */
  private String checkPagename(String message) {
    if (comboPagename != null) {
      Object select = comboPagename.getSelectedItem();
      if (select != null) {
        String tmp = select.toString().trim();
        if (!"".equals(tmp)) {
          return tmp;
        }
      }
    }
    if (message != null) {
      displayWarning(message);
    }
    return null;
  }

  /**
   * Action called when Internal links button is pressed.
   */
  public void actionInternalLinks() {
    // Create menu for internal links
    JPopupMenu menu = new JPopupMenu();
    addItemInInternalLinks(menu, PageListWorker.Mode.INTERNAL_LINKS_MAIN);
    addItemInInternalLinks(menu, PageListWorker.Mode.INTERNAL_LINKS_TALKPAGES_CONVERTED);
    menu.show(
        buttonInternalLinks,
        0,
        buttonInternalLinks.getHeight());
  }

  /**
   * Add an item to the generate lists menu.
   * 
   * @param menu Menu.
   * @param mode List to be generated.
   */
  private void addItemInInternalLinks(JPopupMenu menu, PageListWorker.Mode mode) {
    JMenuItem item = Utilities.createJMenuItem(mode.getTitle(), true);
    item.setActionCommand(mode.name());
    item.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionInternalLinks", "actionCommand"));
    menu.add(item);
  }

  /**
   * Action called when Internal Links button is pressed.
   */
  public void actionInternalLinks(String mode) {
    String pageName = checkPagename(GT._(
        "You must input a page name for retrieving the list of internal links"));
    if ((pageName == null) || (mode == null)) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        pageName);
    config.save();
    new PageListWorker(
        getWikipedia(), this, DataManager.getPage(getWikipedia(), pageName, null, null, null),
        Collections.singletonList(pageName),
        PageListWorker.Mode.valueOf(mode), false,
        GT._("Internal links in {0}", pageName)).start();
  }

  /**
   * Action called when Search Titles button is pressed.
   */
  public void actionSearchTitles() {
    String pageName = checkPagename(GT._("You must input a page name"));
    if (pageName == null) {
      return;
    }
    new PageListWorker(
        getWikipedia(), this, null,
        Collections.singletonList(pageName),
        PageListWorker.Mode.SEARCH_TITLES, false,
        GT._("Search results for {0}", pageName)).start();
  }

  /**
   * Action called when Back links button is pressed.
   */
  public void actionBackLinks() {
    String pageName = checkPagename(GT._(
        "You must input a page name for retrieving the list of backlinks"));
    if (pageName == null) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        pageName);
    config.save();
    Page page = DataManager.getPage(
        getWikipedia(), pageName, null, null, null);
    new PageListWorker(
        getWikipedia(), this, page,
        Collections.singletonList(pageName),
        PageListWorker.Mode.BACKLINKS, false,
        GT._("Links to {0}", pageName)).start();
  }

  /**
   * Action called when Category Members button is pressed.
   */
  public void actionCategoryMembers() {
    String pageName = checkPagename(GT._(
        "You must input a page name for retrieving the list of category members"));
    if (pageName == null) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        pageName);
    config.save();
    String title = getWikipedia().getWikiConfiguration().getPageTitle(
        Namespace.CATEGORY, pageName); 
    Page page = DataManager.getPage(
        getWikipedia(), title, null, null, null);
    new PageListWorker(
        getWikipedia(), this, page,
        Collections.singletonList(title),
        PageListWorker.Mode.CATEGORY_MEMBERS, false,
        GT._("Category members of {0}", title)).start();
  }

  /**
   * Action called when Embedded In button is pressed.
   */
  public void actionEmbeddedIn() {
    String pageName = checkPagename(GT._(
        "You must input a page name for retrieving the list of page in which it is embedded"));
    if (pageName == null) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        pageName);
    config.save();
    String title = getWikipedia().getWikiConfiguration().getPageTitle(
        Namespace.TEMPLATE, pageName);
    Page page = DataManager.getPage(
        getWikipedia(), title, null, null, null);
    new PageListWorker(
        getWikipedia(), this, page,
        Collections.singletonList(title),
        PageListWorker.Mode.EMBEDDED_IN, false,
        GT._("Template {0} embedded in", title)).start();
  }

  /**
   * Action called when Current Disambiguation List is pressed.
   */
  public void actionCurrentDabList() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    List<String> currentDabList = configuration.getStringList(WPCConfigurationStringList.CURRENT_DAB_LIST);
    if ((currentDabList == null) ||
        (currentDabList.isEmpty())) {
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(),
          WPCConfigurationStringList.CURRENT_DAB_LIST.getAttributeName());
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        currentDabList,
        PageListWorker.Mode.INTERNAL_LINKS_MAIN, false,
        GT._("Current disambiguation list")).start();
  }

  /**
   * Action called when "Pages with most disambiguation links" is pressed.
   */
  public void actionMostDabLinks() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    List<String> mostDabLinks = configuration.getStringList(WPCConfigurationStringList.MOST_DAB_LINKS);
    if ((mostDabLinks == null) ||
        (mostDabLinks.isEmpty())) {
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(),
          WPCConfigurationStringList.MOST_DAB_LINKS.getAttributeName());
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        mostDabLinks,
        PageListWorker.Mode.CATEGORY_MEMBERS_ARTICLES,
        false,
        GT._("Pages with many disambiguation links")).start();
  }

  /**
   * Action called when Help Requested On is pressed.
   */
  public void actionHelpRequestedOn() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    List<Page> templates = configuration.getTemplatesForHelpRequested();
    if ((templates == null) ||
        (templates.isEmpty())) {
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(),
          WPCConfigurationStringList.TEMPLATES_FOR_HELP_REQUESTED.getAttributeName());
      return;
    }

    List<String> pageNames = new ArrayList<String>();
    for (Page template : templates) {
      pageNames.add(template.getTitle());
    }
    new PageListWorker(
        wikipedia, this, null,
        pageNames, PageListWorker.Mode.EMBEDDED_IN, false,
        GT._("Help requested on...")).start();
  }

  /**
   * Action called when Abuse Filters button is pressed.
   */
  public void actionAbuseFilters() {
    try {
      API api = APIFactory.getAPI();
      List<AbuseFilter> abuseFilters = api.retrieveAbuseFilters(getWikipedia());
      if ((abuseFilters != null) && (abuseFilters.size() > 0)) {
        Object filter = Utilities.askForValue(
            getParentComponent(),
            GT._("What abuse filter are you interested in?"),
            abuseFilters.toArray(), abuseFilters.get(0));
        if ((filter != null) && (filter instanceof AbuseFilter)) {
          AbuseFilter abuseFilter = (AbuseFilter) filter;
          List<Page> pages = api.retrieveAbuseLog(
              getWikipedia(), abuseFilter.getId(), null);
          if ((pages != null) && (!pages.isEmpty())) {
            List<String> pageNames = new ArrayList<String>(pages.size());
            for (Page page : pages) {
              if (!pageNames.contains(page.getTitle())) {
                pageNames.add(page.getTitle());
              }
            }
            new PageListWorker(
                getWikipedia(), this, null,
                pageNames, PageListWorker.Mode.DIRECT, false,
                GT._("Hits for filter {0}", filter.toString())).start();
          }
        }
      }
    } catch (APIException e) {
      displayError(e);
    }
  }

  /**
   * Action called when Special Lists button is pressed.
   */
  public void actionSpecialLists() {
    // Create menu for special lists
    JPopupMenu menu = new JPopupMenu();
    for (EnumQueryPage query : EnumQueryPage.values()) {
      JMenuItem item = new JMenuItem(query.getName());
      item.setActionCommand(query.getCode());
      item.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionSpecialList", "actionCommand"));
      menu.add(item);
    }
    menu.show(
        buttonSpecialLists,
        0,
        buttonSpecialLists.getHeight());
  }

  /**
   * Action called to get a special list.
   * 
   * @param code Query code.
   */
  public void actionSpecialList(String code) {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    EnumQueryPage query = EnumQueryPage.findByCode(code);
    if (query == null) {
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        Collections.singletonList(code),
        PageListWorker.Mode.QUERY_PAGE, false,
        query.getName()).start();
  }

  /**
   * Action called when Linter Categories button is pressed.
   */
  public void actionLinterCategories() {
    WikiConfiguration config = getWiki().getWikiConfiguration();
    List<LinterCategory> categories = config.getLinterCategories();
    if ((categories == null) || (categories.isEmpty())) {
      displayWarning("The Linter extension is not available on this wiki.");
      return;
    }

    // Create menu for linter categories
    Collections.sort(categories);
    JPopupMenu menu = new JPopupMenu();
    String previousLevel = null;
    for (LinterCategory category : categories) {
      String level = category.getLevelName(config);
      if ((previousLevel == null) || (!previousLevel.equals(level))) {
        JMenuItem separator = new JMenuItem(level);
        separator.setEnabled(false);
        menu.add(separator);
        previousLevel = level;
      }
      JMenu subMenu = new JMenu(category.getCategoryName(config));
      JMenuItem item = new JMenuItem(GT._("All namespaces"));
      item.setActionCommand(category.getCategory());
      item.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionLinterCategory", "actionCommand"));
      subMenu.add(item);
      List<Namespace> namespaces = getWiki().getWikiConfiguration().getNamespaces();
      if ((namespaces != null) && (namespaces.size() > 0)) {
        subMenu.addSeparator();
        for (Namespace namespace : namespaces) {
          item = new JMenuItem(namespace.getTitle());
          item.setActionCommand(category.getCategory() + "/" + namespace.getId().toString());
          item.addActionListener(EventHandler.create(
              ActionListener.class, this, "actionLinterCategory", "actionCommand"));
          subMenu.add(item);
        }
      }
      menu.add(subMenu);
    }
    menu.show(
        buttonLinterCategories,
        0,
        buttonLinterCategories.getHeight());
  }

  /**
   * Action called to get a linter category.
   * 
   * @param category Linter category.
   */
  public void actionLinterCategory(String category) {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    List<String> elements = new ArrayList<>();
    int separator = category.indexOf('/');
    if (separator > 0) {
      elements.add(category.substring(0, separator));
      elements.add(category.substring(separator + 1));
    } else {
      elements.add(category);
    }
    new PageListWorker(
        wikipedia, this, null,
        elements,
        PageListWorker.Mode.LINTER_CATEGORY, false,
        category).start();
  }

  /**
   * Action called when All disambiguations pages is pressed.
   */
  public void actionAllDab() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        null, PageListWorker.Mode.ALL_DAB_PAGES, false,
        GT._("All disambiguations pages")).start();
  }

  /**
   * Action called when Check Wiki is pressed. 
   */
  public void actionCheckWiki() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    if (!wikipedia.getCWConfiguration().isProjectAvailable()) {
      Utilities.displayMissingConfiguration(getParentComponent(), null);
      return;
    }
    Controller.runCheckWikiProject(getWikipedia());
  }

  /**
   * Action called when Bot Tools is pressed. 
   */
  public void actionBotTools() {
    Controller.runBotTools(getWikipedia());
  }
  
  /**
   * Action called when Random page button is pressed.
   */
  public void actionRandomPage() {
    new RandomPageWorker(getWikipedia(), this, comboPagename).start();
  }

  /**
   * Action called when Add page button is pressed.
   */
  public void actionAddPage() {
    String pageName = checkPagename(null);
    if (pageName == null) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    List<String> interestingPages = config.getStringList(
        null, Configuration.ARRAY_INTERESTING_PAGES);
    if (interestingPages == null) {
      interestingPages = new ArrayList<String>();
    }
    if (!interestingPages.contains(pageName)) {
      interestingPages.add(pageName);
      Collections.sort(interestingPages);
      config.setStringList(null, Configuration.ARRAY_INTERESTING_PAGES, interestingPages);
      comboPagename.removeAllItems();
      for (String page : interestingPages) {
        comboPagename.addItem(page);
      }
    }
  }

  /**
   * Action called when Remove page button is pressed.
   */
  public void actionRemovePage() {
    String pageName = checkPagename(null);
    if (pageName == null) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    List<String> interestingPages = config.getStringList(
        null, Configuration.ARRAY_INTERESTING_PAGES);
    if (interestingPages == null) {
      return;
    }
    if (interestingPages.contains(pageName)) {
      interestingPages.remove(pageName);
      Collections.sort(interestingPages);
      config.setStringList(null, Configuration.ARRAY_INTERESTING_PAGES, interestingPages);
      comboPagename.removeAllItems();
      for (String page : interestingPages) {
        comboPagename.addItem(page);
      }
    }
  }

  /**
   * Action called when Generate List button is pressed.
   */
  public void actionGenerateLists() {
    // Create menu for generating lists
    JPopupMenu menu = new JPopupMenu();
    addItemInGenerateLists(menu, PageListWorker.Mode.MISSING_TEMPLATES);
    addItemInGenerateLists(menu, PageListWorker.Mode.PROTECTED_TITLES);
    menu.show(
        buttonGenerateLists,
        0,
        buttonGenerateLists.getHeight());
  }

  /**
   * Add an item to the generate lists menu.
   * 
   * @param menu Menu.
   * @param mode List to be generated.
   */
  private void addItemInGenerateLists(JPopupMenu menu, PageListWorker.Mode mode) {
    JMenuItem item = Utilities.createJMenuItem(mode.getTitle(), true);
    item.setActionCommand(mode.name());
    item.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionGenerateList", "actionCommand"));
    menu.add(item);
  }

  /**
   * Action called to generate a list.
   * 
   * @param name List name.
   */
  public void actionGenerateList(String name) {
    EnumWikipedia wiki = getWikipedia();
    if (wiki == null) {
      return;
    }
    PageListWorker.Mode mode = PageListWorker.Mode.valueOf(name);
    if (mode == null) {
      return;
    }
    new PageListWorker(
        wiki, this, null, null,
        mode, false, mode.getTitle()).start();
  }

  /**
   * Action called when local watch list button is pressed.
   */
  public void actionWatchlistLocal() {
    Configuration config = Configuration.getConfiguration();
    List<String> pageNames = config.getStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        pageNames, PageListWorker.Mode.DIRECT, true,
        GT._("Local Watchlist")).start();
  }

  /**
   * Action called when watch list button is pressed.
   */
  public void actionWatchlist() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        null, PageListWorker.Mode.WATCH_LIST, true,
        GT._("Watch list")).start();
  }

  /**
   * Action called when Load List button is pressed.
   */
  public void actionLoadList() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }

    // Ask which file should be loaded
    JFileChooser chooser = new JFileChooser();
    FileNameExtensionFilter filter = new FileNameExtensionFilter(
        "Text files", "txt");
    chooser.setFileFilter(filter);
    int returnVal = chooser.showOpenDialog(this.getParentComponent());
    if (returnVal != JFileChooser.APPROVE_OPTION) {
       return;
    }
    File chosenFile = chooser.getSelectedFile();
    if ((chosenFile == null) ||
        !chosenFile.isFile() ||
        !chosenFile.canRead()){
      return;
    }

    // Ask in which format the file is
    String[] values = {
        GT._("Unformatted list of page names"),
        GT._("Internal links in a formatted list")
    };
    String message =
        GT._("The file must be encoded in UTF-8 to be read correctly.") + "\n" +
        GT._("In which format is the file?");
    String value = Utilities.askForValue(
        getParentComponent(), message, values, true, values[0], (StringChecker) null);
    if (value == null) {
      return;
    }
    int choice = 0;
    for (int i = 0; i < values.length; i++) {
      if (value.equals(values[i])) {
        choice = i;
      }
    }

    // Read file
    List<String> pages = new ArrayList<String>();
    BufferedReader reader = null;
    String line = null;
    try {
      reader = new BufferedReader(new InputStreamReader(new FileInputStream(chosenFile), "UTF8"));
      switch (choice) {
      case 0: // Unformatted list
        while ((line = reader.readLine()) != null) {
          if (line.trim().length() > 0) {
            pages.add(line);
          }
        }
        break;

      case 1: // Formatted list with internal links
        StringBuilder buffer = new StringBuilder();
        while ((line = reader.readLine()) != null) {
          if (buffer.length() > 0) {
            buffer.append('\n');
          }
          buffer.append(line);
        }
        Page tmpPage = DataManager.getPage(getWiki(), chosenFile.getName(), null, null, null);
        String contents = buffer.toString();
        tmpPage.setContents(contents);
        PageAnalysis analysis = tmpPage.getAnalysis(contents, false);
        List<PageElementInternalLink> links = analysis.getInternalLinks();
        for (PageElementInternalLink link : links) {
          String target = link.getLink();
          if (target.startsWith(":")) {
            target = target.substring(1);
          }
          if (!pages.contains(target)) {
            pages.add(target);
          }
        }
        break;
      }
      new PageListWorker(
          wikipedia, this, null,
          pages, PageListWorker.Mode.DIRECT, true,
          GT._("List")).start();
    } catch (IOException e) {
      //
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (Exception e) {
          //
        }
      }
    }
  }

  /**
   * Action called when Random pages button is pressed.
   */
  public void actionRandom() {
    JPopupMenu menu = new JPopupMenu();
    JMenuItem item = new JMenuItem(GT._("Random pages"));
    item.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRandomPages"));
    menu.add(item);
    item = new JMenuItem(GT._("Random redirect pages"));
    item.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRandomRedirects"));
    menu.add(item);
    menu.show(
        buttonRandomPages,
        0,
        buttonRandomPages.getHeight());
  }

  /**
   * Action called when Random Pages button is pressed.
   */
  public void actionRandomPages() {
    actionRandomArticles(false);
  }

  /**
   * Action called when Random Redirects button is pressed.
   */
  public void actionRandomRedirects() {
    actionRandomArticles(true);
  }

  /**
   * @param redirects True if redirects are requested.
   */
  private void actionRandomArticles(boolean redirects) {
    final int maxPages = 50;
    int count = 0;
    while ((count < 1) || (count > maxPages)) {
      String answer = askForValue(
          GT._("How many pages do you want?"),
          "20", null);
      if (answer == null) {
        return;
      }
      try {
        count = Integer.parseInt(answer);
      } catch (NumberFormatException e) {
        return;
      }
      if ((count < 1) || (count > maxPages)) {
        displayWarning(GT._(
            "The number of pages must be between {0} and {1}",
            new Object[] { Integer.valueOf(0), Integer.valueOf(maxPages) } ));
      }
    }
    API api = APIFactory.getAPI();
    try {
      List<String> pageNames = new ArrayList<String>(count);
      while (pageNames.size() < count) {
        List<Page> pages = api.getRandomPages(getWikipedia(), count - pageNames.size(), redirects);
        for (int i = 0; i < pages.size(); i++) {
          pageNames.add(pages.get(i).getTitle());
        }
      }
      Collections.sort(pageNames);
      new PageListWorker(
          getWikipedia(), this, null, pageNames,
          PageListWorker.Mode.DIRECT, false,
          GT._("Random pages")).start();
    } catch (APIException e) {
      displayError(e);
      return;
    }
  }

  /**
   * Action called when Language is changed.
   */
  public void actionChangeLanguage() {
    if (comboLanguage.getSelectedItem() instanceof EnumLanguage) {
      EnumLanguage language = (EnumLanguage) comboLanguage.getSelectedItem();
      GT.setCurrentLanguage(language);
    }
  }

  /**
   * Action called when Wiki is changed.
   * 
   * Reset users list based on current wikipedia.
   */
  public void actionChangeWiki() {
    comboUser.removeAllItems();
    comboUser.setSelectedItem("");
    if (comboWikipedia.getSelectedItem() instanceof EnumWikipedia) {
      EnumWikipedia wikipedia = (EnumWikipedia) comboWikipedia.getSelectedItem();
      Configuration configuration = Configuration.getConfiguration();
      Properties users = configuration.getProperties(wikipedia, Configuration.PROPERTIES_USERS);
      for (Object user : users.keySet()) {
        comboUser.addItem(user);
      }
      if (comboUser.getItemCount() > 0) {
        comboUser.setSelectedIndex(0);
      }
      String lastUser = configuration.getString(wikipedia, ConfigurationValueString.LAST_USER);
      if (lastUser != null) {
        comboUser.setSelectedItem(lastUser);
      }
    }
    actionChangeUser();
  }

  /**
   * Action called when User is changed.
   * 
   * Reset password based on current wikipedia and user.
   */
  public void actionChangeUser() {
    textPassword.setText("");
    if ((comboWikipedia.getSelectedItem() instanceof EnumWikipedia) &&
        (comboUser.getSelectedItem() instanceof String)) {
      EnumWikipedia wikipedia = (EnumWikipedia) comboWikipedia.getSelectedItem();
      Configuration configuration = Configuration.getConfiguration();
      Properties users = configuration.getProperties(wikipedia, Configuration.PROPERTIES_USERS);
      String password = users.getProperty(comboUser.getSelectedItem().toString());
      if (password != null) {
        textPassword.setText(password);
      }
    }
  }

  /**
   * @return Page.
   * @see org.wikipediacleaner.api.dataaccess.PageProvider#getPage()
   */
  @Override
  public Page getPage() {
    String pageName = checkPagename(null);
    if (pageName != null) {
      return DataManager.getPage(getWikipedia(), pageName, null, null, null);
    }
    return null;
  }
}
