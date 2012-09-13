/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;

import org.lobobrowser.html.HtmlRendererContext;
import org.lobobrowser.html.UserAgentContext;
import org.lobobrowser.html.gui.HtmlPanel;
import org.lobobrowser.html.parser.DocumentBuilderImpl;
import org.lobobrowser.html.test.SimpleUserAgentContext;
import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.ResponseManager;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfiguration;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWHtmlRendererContext;
import org.wikipediacleaner.gui.swing.worker.PageListWorker;
import org.wikipediacleaner.gui.swing.worker.RandomPageWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Main Window of WikipediaCleaner. 
 */
public class MainWindow
  extends BasicWindow
  implements ActionListener, ItemListener {

  private final static String ACTION_ABOUT            = "ABOUT";
  private final static String ACTION_ALL_DAB          = "ALL DAB";
  private final static String ACTION_BOT_TOOLS        = "BOT TOOLS";
  private final static String ACTION_CAT_MEMBERS      = "CAT_MEMBERS";
  private final static String ACTION_CHECK_WIKI       = "CHECK WIKI";
  private final static String ACTION_CONTRIBUTIONS    = "CONTRIBUTIONS";
  private final static String ACTION_CURRENT_DAB_LIST = "CURRENT DAB LIST";
  private final static String ACTION_DEMO             = "DEMO";
  private final static String ACTION_DISAMBIGUATION   = "DISAMBIGUATION";
  private final static String ACTION_EMBEDDED_IN      = "EMBEDDED IN";
  private final static String ACTION_FULL_ANALYSIS    = "FULL ANALYSIS";
  private final static String ACTION_HELP             = "HELP";
  private final static String ACTION_HELP_REQUESTED   = "HELP_REQUESTED";
  private final static String ACTION_IDEA             = "IDEA";
  private final static String ACTION_INTERNAL_LINKS   = "INTERNAL LINKS";
  private final static String ACTION_LOGIN            = "LOGIN";
  private final static String ACTION_LOGOUT           = "LOGOUT";
  private final static String ACTION_MOST_DAB_LINKS   = "MOST DAB LINKS";
  private final static String ACTION_OPTIONS          = "OPTIONS";
  private final static String ACTION_OPTIONS_SYSTEM   = "OPTIONS SYSTEM";
  private final static String ACTION_OTHER_LANGUAGE   = "OTHER LANGUAGE";
  private final static String ACTION_OTHER_WIKIPEDIA  = "OTHER WIKIPEDIA";
  private final static String ACTION_RANDOM_PAGE      = "RANDOM PAGE";
  private final static String ACTION_RANDOM_PAGES     = "RANDOM PAGES";
  private final static String ACTION_RELOAD_CONFIG    = "RELOAD CONFIG";
  private final static String ACTION_SAVE_PASSWORD    = "SAVE PASSWORD";
  private final static String ACTION_SEARCH_TITLES    = "SEARCH TITLES";
  private final static String ACTION_UPDATE_DAB       = "UPDATE DAB WARNING";
  private final static String ACTION_WATCH_LIST_LOCAL = "WATCH LIST LOCAL";
  private final static String ACTION_WATCH_LIST       = "WATCH LIST";

  public final static Integer WINDOW_VERSION = Integer.valueOf(3);

  private final static String URL_OTHER_LANGUAGE  = "http://fr.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner#Other_Language";
  private final static String URL_OTHER_WIKIPEDIA = "http://fr.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner#Other_Wikipedia";
  private final static String URL_TALK_PAGE       = "http://fr.wikipedia.org/wiki/Discussion_Utilisateur:NicoV/Wikipedia_Cleaner";

  private JComboBox comboWikipedia;
  private JComboBox comboLanguage;
  private JComboBox comboUser;
  private JPasswordField textPassword;
  private char echoPassword = '*';
  private ButtonGroup groupSaveUsernamePassword;
  private JRadioButton radSavePassword;
  private JRadioButton radSaveUsername;
  private JRadioButton radSaveNothing;
  private JButton buttonLogin;
  private JButton buttonDemo;
  private JButton buttonLogout;
  private JButton buttonHelp;

  private JButton buttonCurrentDabList;
  private JButton buttonMostDabLinks;
  private JButton buttonCheckWiki;
  private JButton buttonHelpRequested;
  private JButton buttonAllDab;
  private JButton buttonWatchlistLocal;
  private JButton buttonWatchlist;
  private JButton buttonRandomPages;
  private JButton buttonBotTools;

  JTextField textPagename;
  private JButton buttonFullAnalysis;
  private JButton buttonDisambiguation;
  private JButton buttonSearchTitles;
  private JButton buttonEmbeddedIn;
  private JButton buttonInternalLinks;
  private JButton buttonCategoryMembers;
  private JButton buttonUpdateDabWarning;
  private JButton buttonRandomPage;
  private JButton buttonContributions;

  private JButton buttonOptions;
  private JButton buttonOptionsSystem;
  private JButton buttonReloadOptions;
  private JButton buttonIdea;
  private JButton buttonAbout;

  boolean logged = false;
  boolean userLogged = false;

  private static class MainWindowListener implements BasicWindowListener {

    public MainWindowListener() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.basic.BasicWindowListener#initializeWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
     */
    public void initializeWindow(@SuppressWarnings("unused") BasicWindow window) {
      // Nothing to do
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.basic.BasicWindowListener#displayWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
     */
    public void displayWindow(BasicWindow window) {
      Configuration config = Configuration.getConfiguration();
      config.checkVersion(window.getParentComponent());
    }
    
  }

  /**
   * Create and display a MainWindow.
   */
  public static void createMainWindow() {
    createWindow(
        "MainWindow",
        null,
        JFrame.EXIT_ON_CLOSE,
        MainWindow.class,
        new MainWindowListener());
  }

  /* (non-Javadoc)
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
      constraints.gridwidth = 3;
      constraints.gridx = 0;
      constraints.weighty = 0;
      panel.add(createMessageComponents(), constraints);
      constraints.gridy++;
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
    buttonOptionsSystem.setEnabled(logged);
    buttonReloadOptions.setEnabled(logged);

    buttonCurrentDabList.setEnabled(logged);
    buttonMostDabLinks.setEnabled(logged);
    buttonCheckWiki.setEnabled(logged);
    buttonHelpRequested.setEnabled(logged);
    buttonAllDab.setEnabled(logged);
    buttonWatchlistLocal.setEnabled(logged);
    buttonWatchlist.setEnabled(logged);
    buttonRandomPages.setEnabled(logged);
    buttonBotTools.setEnabled(userLogged);

    textPagename.setEnabled(logged);
    buttonFullAnalysis.setEnabled(logged);
    buttonDisambiguation.setEnabled(logged);
    buttonSearchTitles.setEnabled(logged);
    buttonInternalLinks.setEnabled(logged);
    buttonCategoryMembers.setEnabled(logged);
    buttonEmbeddedIn.setEnabled(logged);
    buttonUpdateDabWarning.setEnabled(logged);
    buttonRandomPage.setEnabled(logged);
    buttonContributions.setEnabled(logged);
  }

  /**
   * @return Message components.
   */
  private Component createMessageComponents() {
    JPanel panel = new JPanel(new GridLayout(1, 0));
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Message")));
    HtmlPanel textMessage = new HtmlPanel();
    UserAgentContext ucontextMessage = new SimpleUserAgentContext();
    HtmlRendererContext rcontextMessage = new MWHtmlRendererContext(
        textMessage, ucontextMessage);
    textMessage.setPreferredSize(new Dimension(500, 150));
    textMessage.setMinimumSize(new Dimension(100, 100));
    DocumentBuilderImpl dbi = new DocumentBuilderImpl(
        ucontextMessage, rcontextMessage);
    InputSource is = new InputSource(new StringReader(Version.MESSAGE));
    is.setSystemId(EnumWikipedia.EN.getConfiguration().getHelpURL());
    try {
      textMessage.setDocument(dbi.parse(is), rcontextMessage);
    } catch (SAXException e) {
      // Nothing
    } catch (IOException e) {
      // Nothing
    }
    panel.add(textMessage);
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
    comboWikipedia = new JComboBox(EnumWikipedia.getList().toArray());
    comboWikipedia.setEditable(false);
    comboWikipedia.setSelectedItem(defaultWikipedia);
    comboWikipedia.addItemListener(this);
    JLabel labelWikipedia = Utilities.createJLabel(GT._("&Wikipedia"));
    labelWikipedia.setLabelFor(comboWikipedia);
    labelWikipedia.setHorizontalAlignment(SwingConstants.TRAILING);
    JToolBar toolbarWikipedia = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarWikipedia.setFloatable(false);
    JButton buttonWikipediaInfo = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.SMALL,
        GT._("Other Wikipedia"), false);
    buttonWikipediaInfo.setActionCommand(ACTION_OTHER_WIKIPEDIA);
    buttonWikipediaInfo.addActionListener(this);
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
    comboLanguage = new JComboBox(EnumLanguage.getList().toArray());
    comboLanguage.setEditable(false);
    comboLanguage.setSelectedItem(configuration.getLanguage());
    comboLanguage.addItemListener(this);
    JLabel labelLanguage = Utilities.createJLabel(GT._("Lan&guage"));
    labelLanguage.setLabelFor(comboLanguage);
    labelLanguage.setHorizontalAlignment(SwingConstants.TRAILING);
    JToolBar toolbarLanguage = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarLanguage.setFloatable(false);
    JButton buttonLanguageInfo = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.SMALL,
        GT._("Other Language"), false);
    buttonLanguageInfo.setActionCommand(ACTION_OTHER_LANGUAGE);
    buttonLanguageInfo.addActionListener(this);
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
    comboUser = new JComboBox();
    comboUser.setEditable(true);
    comboUser.addItemListener(this);
    JLabel labelUsername = Utilities.createJLabel(GT._("&User name :"));
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
    JLabel labelPassword = Utilities.createJLabel(GT._("&Password :"));
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
    buttonLogin = Utilities.createJButton(GT._("&Login"));
    buttonLogin.setActionCommand(ACTION_LOGIN);
    buttonLogin.addActionListener(this);
    buttonToolbar.add(buttonLogin);
    buttonToolbar.addSeparator();
    buttonDemo = Utilities.createJButton(GT._("&Demo"));
    buttonDemo.setActionCommand(ACTION_DEMO);
    buttonDemo.addActionListener(this);
    buttonToolbar.add(buttonDemo);
    buttonToolbar.addSeparator();
    buttonLogout = Utilities.createJButton(GT._("L&ogout"));
    buttonLogout.setActionCommand(ACTION_LOGOUT);
    buttonLogout.addActionListener(this);
    buttonToolbar.add(buttonLogout);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    panel.add(buttonToolbar, constraints);
    constraints.gridy++;

    // Buttons
    buttonToolbar = new JToolBar(SwingConstants.HORIZONTAL);
    buttonToolbar.setFloatable(false);
    buttonHelp = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.NORMAL,
        GT._("Help (Alt + &H)"), false);
    buttonHelp.setActionCommand(ACTION_HELP);
    buttonHelp.addActionListener(this);
    buttonToolbar.add(buttonHelp);
    buttonOptions = Utilities.createJButton(
        "gnome-preferences-other.png", EnumImageSize.NORMAL,
        GT._("Options (Alt + &O)"), false);
    buttonOptions.setActionCommand(ACTION_OPTIONS);
    buttonOptions.addActionListener(this);
    buttonToolbar.add(buttonOptions);
    buttonOptionsSystem = Utilities.createJButton(
        "gnome-preferences-system.png", EnumImageSize.NORMAL,
        GT._("System Options (Alt + &Y)"), false);
    buttonOptionsSystem.setActionCommand(ACTION_OPTIONS_SYSTEM);
    buttonOptionsSystem.addActionListener(this);
    buttonToolbar.add(buttonOptionsSystem);
    buttonReloadOptions = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        GT._("Reload system options"), false);
    buttonReloadOptions.setActionCommand(ACTION_RELOAD_CONFIG);
    buttonReloadOptions.addActionListener(this);
    buttonToolbar.add(buttonReloadOptions);
    buttonToolbar.addSeparator();
    buttonIdea = Utilities.createJButton(GT._("&Idea ? Bug ?"));
    buttonIdea.setActionCommand(ACTION_IDEA);
    buttonIdea.addActionListener(this);
    buttonToolbar.add(buttonIdea);
    buttonToolbar.addSeparator();
    buttonAbout = Utilities.createJButton(GT._("About"));
    buttonAbout.setActionCommand(ACTION_ABOUT);
    buttonAbout.addActionListener(this);
    buttonAbout.setEnabled(false);
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
        GT._("Save user name and password"),
        (saveUser == ConfigurationConstants.VALUE_SAVE_USER_BOTH));
    radSavePassword.setActionCommand(ACTION_SAVE_PASSWORD);
    radSavePassword.addActionListener(this);
    groupSaveUsernamePassword.add(radSavePassword);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.gridwidth = 2;
    panel.add(radSavePassword, constraints);
    constraints.gridy++;
    radSaveUsername = Utilities.createJRadioButton(
        GT._("Save user name only"),
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

    resetUsersList();

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
    textPagename = Utilities.createJTextField(lastPage, 20);
    panel.add(textPagename, constraints);
    constraints.gridx++;

    // Random page button
    JToolBar toolbarRandom = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarRandom.setFloatable(false);
    buttonRandomPage = Utilities.createJButton(
        "commons-nuvola-apps-atlantik.png", EnumImageSize.SMALL,
        GT._("Random page (Alt + &R)"), false);
    buttonRandomPage.setActionCommand(ACTION_RANDOM_PAGE);
    buttonRandomPage.addActionListener(this);
    toolbarRandom.add(buttonRandomPage);
    constraints.weightx = 0;
    panel.add(toolbarRandom, constraints);
    constraints.gridy++;

    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;

    // Full analysis button
    buttonFullAnalysis = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("&Full analysis"), true);
    buttonFullAnalysis.setActionCommand(ACTION_FULL_ANALYSIS);
    buttonFullAnalysis.addActionListener(this);
    panel.add(buttonFullAnalysis, constraints);
    constraints.gridy++;

    // Disambiguation button
    buttonDisambiguation = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("&Disambiguation"), true);
    buttonDisambiguation.setActionCommand(ACTION_DISAMBIGUATION);
    buttonDisambiguation.addActionListener(this);
    panel.add(buttonDisambiguation, constraints);
    constraints.gridy++;

    // Search button
    buttonSearchTitles = Utilities.createJButton(
        "gnome-system-search.png", EnumImageSize.NORMAL,
        GT._("Search in titles"), true);
    buttonSearchTitles.setActionCommand(ACTION_SEARCH_TITLES);
    buttonSearchTitles.addActionListener(this);
    panel.add(buttonSearchTitles, constraints);
    constraints.gridy++;

    // Links button
    buttonInternalLinks = Utilities.createJButton(GT._("Internal links"));
    buttonInternalLinks.setActionCommand(ACTION_INTERNAL_LINKS);
    buttonInternalLinks.addActionListener(this);
    panel.add(buttonInternalLinks, constraints);
    constraints.gridy++;

    // Category members
    buttonCategoryMembers = Utilities.createJButton(
        "commons-nuvola-apps-kpager.png", EnumImageSize.NORMAL,
        GT._("Category members"), true);
    buttonCategoryMembers.setActionCommand(ACTION_CAT_MEMBERS);
    buttonCategoryMembers.addActionListener(this);
    panel.add(buttonCategoryMembers, constraints);
    constraints.gridy++;

    // Embedded in
    buttonEmbeddedIn = Utilities.createJButton(
        "commons-curly-brackets.png", EnumImageSize.NORMAL,
        GT._("Embedded in"), true);
    buttonEmbeddedIn.setActionCommand(ACTION_EMBEDDED_IN);
    buttonEmbeddedIn.addActionListener(this);
    panel.add(buttonEmbeddedIn, constraints);
    constraints.gridy++;

    // Update disambiguation warning
    buttonUpdateDabWarning = Utilities.createJButton(
        "gnome-dialog-warning.png", EnumImageSize.NORMAL,
        GT._("Update disambiguation warning"), true);
    buttonUpdateDabWarning.setActionCommand(ACTION_UPDATE_DAB);
    buttonUpdateDabWarning.addActionListener(this);
    panel.add(buttonUpdateDabWarning, constraints);
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

    // Contributions
    buttonContributions = Utilities.createJButton(
        "gnome-utilities-system-monitor.png", EnumImageSize.NORMAL,
        GT._("Your contributions"), true);
    buttonContributions.setActionCommand(ACTION_CONTRIBUTIONS);
    buttonContributions.addActionListener(this);
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
        GT._("Preload disambiguations pages"), true);
    buttonAllDab.setActionCommand(ACTION_ALL_DAB);
    buttonAllDab.addActionListener(this);
    panel.add(buttonAllDab, constraints);
    constraints.gridy++;

    // Current disambiguation list button
    buttonCurrentDabList = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("&Current Disambiguation List"), true);
    buttonCurrentDabList.setActionCommand(ACTION_CURRENT_DAB_LIST);
    buttonCurrentDabList.addActionListener(this);
    panel.add(buttonCurrentDabList, constraints);
    constraints.gridy++;

    // Pages with most disambiguation links button
    buttonMostDabLinks = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("With many disambiguation links"), true);
    buttonMostDabLinks.setActionCommand(ACTION_MOST_DAB_LINKS);
    buttonMostDabLinks.addActionListener(this);
    panel.add(buttonMostDabLinks, constraints);
    constraints.gridy++;

    // Check Wiki Project button
    buttonCheckWiki = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Project check wikipedia"), true);
    buttonCheckWiki.setActionCommand(ACTION_CHECK_WIKI);
    buttonCheckWiki.addActionListener(this);
    panel.add(buttonCheckWiki, constraints);
    constraints.gridy++;

    // Help requested button
    buttonHelpRequested = Utilities.createJButton(GT._("Help requested on..."));
    buttonHelpRequested.setActionCommand(ACTION_HELP_REQUESTED);
    buttonHelpRequested.addActionListener(this);
    panel.add(buttonHelpRequested, constraints);
    constraints.gridy++;

    // Local watch list button
    buttonWatchlistLocal = Utilities.createJButton(
        "gnome-logviewer.png", EnumImageSize.NORMAL,
        GT._("Local &Watch list"), true);
    buttonWatchlistLocal.setActionCommand(ACTION_WATCH_LIST_LOCAL);
    buttonWatchlistLocal.addActionListener(this);
    panel.add(buttonWatchlistLocal, constraints);
    constraints.gridy++;

    // Watch list button
    buttonWatchlist = Utilities.createJButton(
        "gnome-logviewer.png", EnumImageSize.NORMAL,
        GT._("Watch list"), true);
    buttonWatchlist.setActionCommand(ACTION_WATCH_LIST);
    buttonWatchlist.addActionListener(this);
    panel.add(buttonWatchlist, constraints);
    constraints.gridy++;

    // Random pages button
    buttonRandomPages = Utilities.createJButton(
        "commons-nuvola-apps-atlantik.png", EnumImageSize.NORMAL,
        GT._("Random pages"), true);
    buttonRandomPages.setActionCommand(ACTION_RANDOM_PAGES);
    buttonRandomPages.addActionListener(this);
    panel.add(buttonRandomPages, constraints);
    constraints.gridy++;

    // Bot tools button
    buttonBotTools = Utilities.createJButton(
        "commons-nuvola-apps-kcmsystem.png", EnumImageSize.NORMAL,
        GT._("Bot tools"), true);
    buttonBotTools.setActionCommand(ACTION_BOT_TOOLS);
    buttonBotTools.addActionListener(this);
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
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_LOGIN.equals(e.getActionCommand())) {
      actionLogin();
    } else if (ACTION_DEMO.equals(e.getActionCommand())) {
      actionDemo();
    } else if (ACTION_LOGOUT.equals(e.getActionCommand())) {
      actionLogout();
    } else if (ACTION_HELP.equals(e.getActionCommand())) {
      actionHelp();
    } else if (ACTION_IDEA.equals(e.getActionCommand())) {
      actionIdea();
    } else if (ACTION_ABOUT.equals(e.getActionCommand())) {
      Controller.runAbout();
    } else if (ACTION_SAVE_PASSWORD.equals(e.getActionCommand())) {
      actionSavePassword();
    } else if (ACTION_FULL_ANALYSIS.equals(e.getActionCommand())) {
      actionFullAnalysis();
    } else if (ACTION_DISAMBIGUATION.equals(e.getActionCommand())) {
      actionDisambiguation();
    } else if (ACTION_INTERNAL_LINKS.equals(e.getActionCommand())) {
      actionInternalLinks();
    } else if (ACTION_SEARCH_TITLES.equals(e.getActionCommand())) {
      actionSearchTitles();
    } else if (ACTION_CAT_MEMBERS.equals(e.getActionCommand())) {
      actionCategoryMembers();
    } else if (ACTION_EMBEDDED_IN.equals(e.getActionCommand())) {
      actionEmbeddedIn();
    } else if (ACTION_CURRENT_DAB_LIST.equals(e.getActionCommand())) {
      actionCurrentDabList();
    } else if (ACTION_MOST_DAB_LINKS.equals(e.getActionCommand())) {
      actionMostDabLinks();
    } else if (ACTION_RANDOM_PAGE.equals(e.getActionCommand())) {
      actionRandomPage();
    } else if (ACTION_WATCH_LIST_LOCAL.equals(e.getActionCommand())) {
      actionWatchlistLocal();
    } else if (ACTION_WATCH_LIST.equals(e.getActionCommand())) {
      actionWatchlist();
    } else if (ACTION_OPTIONS.equals(e.getActionCommand())) {
      Controller.runOptions();
    } else if (ACTION_OPTIONS_SYSTEM.equals(e.getActionCommand())) {
      actionOptionsSystem();
    } else if (ACTION_RELOAD_CONFIG.equals(e.getActionCommand())) {
      actionReloadOptions();
    } else if (ACTION_OTHER_LANGUAGE.equals(e.getActionCommand())) {
      actionOtherLanguage();
    } else if (ACTION_OTHER_WIKIPEDIA.equals(e.getActionCommand())) {
      actionOtherWikipedia();
    } else if (ACTION_HELP_REQUESTED.equals(e.getActionCommand())) {
      actionHelpRequestedOn();
    } else if (ACTION_ALL_DAB.equals(e.getActionCommand())) {
      actionAllDab();
    } else if (ACTION_CHECK_WIKI.equals(e.getActionCommand())) {
      actionCheckWiki();
    } else if (ACTION_RANDOM_PAGES.equals(e.getActionCommand())) {
      actionRandomPages();
    } else if (ACTION_BOT_TOOLS.equals(e.getActionCommand())) {
      actionBotTools();
    } else if (ACTION_UPDATE_DAB.equals(e.getActionCommand())) {
      actionUpdateDabWarning();
    } else if (ACTION_CONTRIBUTIONS.equals(e.getActionCommand())) {
      actionContributions();
    }
  }

  /**
   * Action called when Login button is pressed.
   */
  private void actionLogin() {
    actionLoginDemo(true);
  }
  
  /**
   * Action called when Demo button is pressed. 
   */
  private void actionDemo() {
    int answer = displayYesNoWarning(GT._(
        "Demo mode is only available for testing WikiCleaner.\n" +
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
  private void actionLoginDemo(boolean login) {

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
            GT._("You must input your user name before login"),
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

    // Login
    new LoginWorker(
        getWikipedia(), this,
        (EnumLanguage) comboLanguage.getSelectedItem(),
        comboUser.getSelectedItem().toString(),
        textPassword.getPassword(),
        radSavePassword.isSelected() ?
            ConfigurationConstants.VALUE_SAVE_USER_BOTH :
            radSaveUsername.isSelected() ?
                ConfigurationConstants.VALUE_SAVE_USER_NAME :
                ConfigurationConstants.VALUE_SAVE_USER_NONE,
        login, false).start();
  }

  /**
   * Action called when Logout button is pressed.
   */
  private void actionLogout() {
    API api = APIFactory.getAPI();
    api.logout();
    logged = false;
    userLogged = false;
    updateComponentState();
  }

  /**
   * Action called when System Options button is pressed.
   */
  private void actionOptionsSystem() {
    if (Utilities.isDesktopSupported()) {
      EnumWikipedia wikipedia = getWikipedia();
      Utilities.browseURL(wikipedia, wikipedia.getConfigurationPage(), true);
    } else {
      displayUrlMessage(
          GT._("You can learn how to configure WikiCleaner at the following URL:"),
          URL_OTHER_WIKIPEDIA);
    }
  }

  /**
   * Action called when Reload System Options button is pressed.
   */
  private void actionReloadOptions() {
    new LoginWorker(
        getWikipedia(), this,
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
   * Action called when Help button is pressed.
   */
  private void actionHelp() {
    EnumWikipedia wikipedia = getWikipedia();
    String url = EnumWikipedia.EN.getConfiguration().getHelpURL();
    if ((wikipedia != null) && (wikipedia.getConfiguration().getHelpURL() != null)) {
      url = wikipedia.getConfiguration().getHelpURL();
    }
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      displayUrlMessage(
          GT._("You can read the help on Wikipedia Cleaner at the following URL:"),
          url);
    }
  }

  /**
   * Action called when Idea button is pressed.
   */
  private void actionIdea() {
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
  private void actionContributions() {
    EnumWikipedia wikipedia = getWikipedia();
    if ((wikipedia != null) && (wikipedia.getContributions() != null)) {
      Utilities.displayInformationMessage(
          getParentComponent(),
          wikipedia.getContributions().getDescription());
    }
  }
  
  /**
   * Action called when Save Password is changed. 
   */
  private void actionSavePassword() {
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
   * Action called when Full analysis button is pressed.
   */
  private void actionFullAnalysis() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name for running a full analysis"),
          textPagename);
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        textPagename.getText().trim());
    config.save();
    Controller.runFullAnalysis(
        textPagename.getText().trim(), null,
        getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionDisambiguation() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name for running a disambiguation analysis"),
          textPagename);
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        textPagename.getText().trim());
    config.save();
    Controller.runDisambiguationAnalysis(
        textPagename.getText().trim(),
        getWikipedia());
  }

  /**
   * Action called when Update Dab Warning button is pressed.
   */
  private void actionUpdateDabWarning() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name for updating the disambiguation warning"),
          textPagename);
      return;
    }
    String template = getConfiguration().getDisambiguationWarningTemplate();
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You need to define the 'dab_warning_template' property in WikiCleaner configuration."));
    }
    int answer = Utilities.displayYesNoWarning(
        getParentComponent(),
        GT._("Do you want to update the disambiguation warning in talk page ?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(
        getWikipedia(), this,
        Collections.singletonList(DataManager.getPage(getWikipedia(), textPagename.getText(), null, null)));
    worker.start();
  }

  /**
   * Action called when Internal Links button is pressed.
   */
  private void actionInternalLinks() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name for retrieving the list of internal links"),
          textPagename);
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        textPagename.getText().trim());
    config.save();
    new PageListWorker(
        getWikipedia(), this, null,
        Collections.singletonList(textPagename.getText().trim()),
        PageListWorker.Mode.INTERNAL_LINKS, false,
        GT._("Internal links in {0}", textPagename.getText().trim())).start();
  }

  /**
   * Action called when Search Titles button is pressed.
   */
  private void actionSearchTitles() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name"),
          textPagename);
      return;
    }
    new PageListWorker(
        getWikipedia(), this, null,
        Collections.singletonList(textPagename.getText().trim()),
        PageListWorker.Mode.SEARCH_TITLES, false,
        GT._("Search results for {0}", textPagename.getText().trim())).start();
  }

  /**
   * Action called when Category Members button is pressed.
   */
  private void actionCategoryMembers() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name for retrieving the list of category members"),
          textPagename);
      return;
    }
    String pageName = textPagename.getText().trim();
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        pageName);
    config.save();
    Page page = DataManager.getPage(
        getWikipedia(),
        Namespace.getTitle(Namespace.CATEGORY, getWikipedia().getNamespaces(), pageName),
        null, null);
    new PageListWorker(
        getWikipedia(), this, page,
        Collections.singletonList(pageName),
        PageListWorker.Mode.CATEGORY_MEMBERS, false,
        GT._("Category members of {0}", pageName)).start();
  }

  /**
   * Action called when Embedded In button is pressed.
   */
  private void actionEmbeddedIn() {
    if ((textPagename == null) ||
        (textPagename.getText() == null) ||
        ("".equals(textPagename.getText().trim()))) {
      displayWarning(
          GT._("You must input a page name for retrieving the list of page it is embedded in"),
          textPagename);
      return;
    }
    String pageName = textPagename.getText().trim();
    Configuration config = Configuration.getConfiguration();
    config.setString(
        null,
        ConfigurationValueString.PAGE_NAME,
        pageName);
    config.save();
    Page page = DataManager.getPage(
        getWikipedia(),
        Namespace.getTitle(Namespace.TEMPLATE, getWikipedia().getNamespaces(), pageName),
        null, null);
    new PageListWorker(
        getWikipedia(), this, page,
        Collections.singletonList(pageName),
        PageListWorker.Mode.EMBEDDED_IN, false,
        GT._("Template {0} embedded in", pageName)).start();
  }

  /**
   * Action called when Current Disambiguation List is pressed.
   */
  private void actionCurrentDabList() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    if ((configuration.getCurrentDisambiguationList() == null) ||
        (configuration.getCurrentDisambiguationList().isEmpty())) {
      String url = URL_OTHER_WIKIPEDIA;
      displayUrlMessage(
          GT._(
              "There's no known list of disambiguation pages for this Wikipedia.\n" +
              "You can learn how to configure WikiCleaner at the following URL:"),
          url);
      if (Utilities.isDesktopSupported()) {
        Utilities.browseURL(url);
      }
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        configuration.getCurrentDisambiguationList(),
        PageListWorker.Mode.INTERNAL_LINKS, false,
        GT._("Current disambiguation list")).start();
  }

  /**
   * Action called when "Pages with most disambiguation links" is pressed.
   */
  private void actionMostDabLinks() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    if ((configuration.getMostDisambiguationLinks() == null) ||
        (configuration.getMostDisambiguationLinks().isEmpty())) {
      String url = URL_OTHER_WIKIPEDIA;
      displayUrlMessage(
          GT._(
              "There's no known list of pages containing many disambiguation links for this Wikipedia.\n" +
              "You can learn how to configure WikiCleaner at the following URL:"),
          url);
      if (Utilities.isDesktopSupported()) {
        Utilities.browseURL(url);
      }
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        configuration.getMostDisambiguationLinks(),
        PageListWorker.Mode.CATEGORY_MEMBERS_ARTICLES,
        false,
        GT._("Pages with many disambiguation links")).start();
  }

  /**
   * Action called when Help Requested On is pressed.
   */
  private void actionHelpRequestedOn() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    WPCConfiguration configuration = wikipedia.getConfiguration();
    if (configuration.getTemplatesForHelpRequested() == null) {
      String url = URL_OTHER_WIKIPEDIA;
      displayUrlMessage(
          GT._(
              "There's no known template for requesting help for the Wikipedia.\n" +
              "You can learn how to configure WikiCleaner at the following URL:"),
          url);
      if (Utilities.isDesktopSupported()) {
        Utilities.browseURL(url);
      }
    } else {
      List<String> pageNames = new ArrayList<String>();
      for (Page template : configuration.getTemplatesForHelpRequested()) {
        pageNames.add(template.getTitle());
      }
      new PageListWorker(
          wikipedia, this, null,
          pageNames, PageListWorker.Mode.EMBEDDED_IN, false,
          GT._("Help requested on...")).start();
    }
  }

  /**
   * Action called when All disambiguations pages is pressed.
   */
  private void actionAllDab() {
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
  private void actionCheckWiki() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    if (!wikipedia.getCWConfiguration().isProjectAvailable()) {
      String url = URL_OTHER_WIKIPEDIA;
      displayUrlMessage(
          GT._(
              "There's no known Check Wikipedia Project for this Wikipedia.\n" +
              "You can learn how to configure WikiCleaner at the following URL:"),
          url);
      if (Utilities.isDesktopSupported()) {
        Utilities.browseURL(url);
      }
      return;
    }
    Controller.runCheckWikiProject(getWikipedia());
  }

  /**
   * Action called when Bot Tools is pressed. 
   */
  private void actionBotTools() {
    Controller.runBotTools(getWikipedia());
  }
  
  /**
   * Action called when Random page button is pressed.
   */
  private void actionRandomPage() {
    new RandomPageWorker(getWikipedia(), this, textPagename).start();
  }

  /**
   * Action called when local watch list button is pressed.
   */
  private void actionWatchlistLocal() {
    Configuration config = Configuration.getConfiguration();
    List<String> pageNames = config.getStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    new PageListWorker(
        wikipedia, this, null,
        pageNames, PageListWorker.Mode.DIRECT, true,
        GT._("Local watch list")).start();
  }

  /**
   * Action called when watch list button is pressed.
   */
  private void actionWatchlist() {
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
   * Action called when Random Pages is pressed.
   */
  private void actionRandomPages() {
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
        List<Page> pages = api.getRandomPages(getWikipedia(), count - pageNames.size());
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
   * SwingWorker for login. 
   */
  class LoginWorker extends BasicWorker {

    private final EnumLanguage language;
    private final String username;
    private final char[] password;
    private final int saveUser;
    private final boolean login;
    private final boolean reloadOnly;

    public LoginWorker(
        EnumWikipedia wikipedia,
        BasicWindow window,
        EnumLanguage language,
        String username,
        char[] password,
        int saveUser,
        boolean login,
        boolean reloadOnly) {
      super(wikipedia, window);
      this.language = language;
      this.username = username.trim();
      this.password = password;
      this.saveUser = saveUser;
      this.login = login;
      this.reloadOnly = reloadOnly;
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
     */
    @Override
    public void finished() {
      super.finished();
      if (password != null) {
        for (int i = 0; i < password.length; i++) {
          password[i] = '\0';
        }
      }
      textPagename.requestFocusInWindow();
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
     */
    @Override
    public Object construct() {
      try {
        setText(GT._("Retrieving MediaWiki API"));
        API api = APIFactory.getAPI();

        // Login
        if (!reloadOnly) {
          setText(GT._("Login"));
          LoginResult result = api.login(getWikipedia(), username, new String(password), login);
          if (login) {
            if ((result == null) || (!result.isLoginSuccessful())) {
              throw new APIException("Login unsuccessful: " + ((result != null) ? result.toString() : ""));
            }
          }
          logged = true;
          userLogged = login;
        }

        // Load configuration
        setText(GT._("Loading configuration"));
        api.loadConfiguration(getWikipedia(), username);

        // Saving settings
        Configuration configuration = Configuration.getConfiguration();
        configuration.setWikipedia(getWikipedia());
        configuration.setLanguage(language);
        if (login && !reloadOnly) {
          Properties props = configuration.getProperties(getWikipedia(), Configuration.PROPERTIES_USERS);
          if (saveUser == ConfigurationConstants.VALUE_SAVE_USER_NONE) {
            props.remove(username);
            configuration.setString(getWikipedia(), ConfigurationValueString.LAST_USER, (String) null);
          } else {
            props.setProperty(
                username,
                (saveUser == ConfigurationConstants.VALUE_SAVE_USER_BOTH) ? new String(password) : "");
            configuration.setString(getWikipedia(), ConfigurationValueString.LAST_USER, username);
          }
          configuration.setProperties(getWikipedia(), Configuration.PROPERTIES_USERS, props);
          configuration.setInt(
              null,
              ConfigurationValueInteger.SAVE_USER,
              saveUser);
        }
        Configuration.getConfiguration().save();

        // Retrieving disambiguation templates
        setText(GT._("Retrieving disambiguation templates"));
        getWikipedia().initDisambiguationTemplates(api);

        // Retrieving suggestions for text replacements
        setText(GT._("Retrieving suggestions for text replacements"));
        getConfiguration().initSuggestions(api, reloadOnly);

        // Retrieving general Check Wiki configuration
        final CWConfiguration cwConfiguration = getWikipedia().getCWConfiguration();
        String code = getWikipedia().getSettings().getCodeCheckWiki().replace("-", "_");
        try {
          setText(GT._("Retrieving Check Wiki configuration"));
          ResponseManager manager = new ResponseManager() {
            
            public void manageResponse(InputStream stream) throws IOException, APIException {
              if (stream != null) {
                cwConfiguration.setGeneralConfiguration(
                    new InputStreamReader(stream, "UTF-8"));
              }
            }
          };
          APIFactory.getToolServer().sendGet(
              "~sk/checkwiki/" + code + "/" + code + "_translation.txt",
              manager);
        } catch (APIException e) {
          System.err.println("Error retrieving Check Wiki configuration: " + e.getMessage());
        }

        // Retrieving specific Check Wiki configuration
        try {
          setText(GT._("Retrieving Check Wiki configuration"));
          if (getWikipedia().getCWConfiguration().getTranslationPage() != null) {
            MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
            Page page = DataManager.getPage(
                getWikipedia(),
                cwConfiguration.getTranslationPage(),
                null, null);
            mw.retrieveContents(getWikipedia(), page, true, false, false);
            if (Boolean.TRUE.equals(page.isExisting())) {
              cwConfiguration.setWikiConfiguration(new StringReader(page.getContents()));
            }
          }
        } catch (APIException e) {
          System.err.println("Error retrieving Check Wiki configuration: " + e.getMessage());
        }

        // Retrieving white lists
        HashMap<String, Page> whiteListPages = new HashMap<String, Page>();
        for (int i = 0; i < CWConfiguration.MAX_ERROR_NUMBER; i++) {
          CWConfigurationError error = cwConfiguration.getErrorConfiguration(i);
          if ((error != null) && (error.getWhiteListPageName() != null)) {
            Page page = DataManager.getPage(getWikipedia(), error.getWhiteListPageName(), null, null);
            whiteListPages.put(error.getWhiteListPageName(), page);
          }
        }
        if (whiteListPages.size() > 0) {
          api.retrieveLinks(getWikipedia(), whiteListPages.values());
          for (int i = 0; i < CWConfiguration.MAX_ERROR_NUMBER; i++) {
            CWConfigurationError error = cwConfiguration.getErrorConfiguration(i);
            if ((error != null) && (error.getWhiteListPageName() != null)) {
              Page page = whiteListPages.get(error.getWhiteListPageName());
              error.setWhiteList(page);
            }
          }
        }
        CheckErrorAlgorithms.initializeAlgorithms(getWikipedia());
      } catch (APIException e) {
        return e;
      }
      return null;
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  public void itemStateChanged(ItemEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }

    // New wikipedia selected: change wikipedia
    if (e.getSource() == comboWikipedia) {
      resetUsersList();
      return;
    }

    // New language selected: change current language
    if (e.getSource() == comboLanguage) {
      if (comboLanguage.getSelectedItem() instanceof EnumLanguage) {
        EnumLanguage language = (EnumLanguage) comboLanguage.getSelectedItem();
        GT.setCurrentLanguage(language);
      }
      return;
    }

    // New user selected: change password
    if (e.getSource() == comboUser) {
      resetPassword();
      return;
    }
  }

  /**
   * Reset users list based on current wikipedia.
   */
  private void resetUsersList() {
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
    resetPassword();
  }

  /**
   * Reset password based on current wikipedia and user.
   */
  private void resetPassword() {
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
}
