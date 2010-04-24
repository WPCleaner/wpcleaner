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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.text.DateFormat;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.DisambiguationListWorker;
import org.wikipediacleaner.gui.swing.worker.EmbeddedInWorker;
import org.wikipediacleaner.gui.swing.worker.RandomPageWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * Main Window of WikipediaCleaner. 
 */
public class MainWindow
  extends BasicWindow
  implements ActionListener, ItemListener {

  private final static String ACTION_ABOUT          = "ABOUT";
  private final static String ACTION_BOT_TOOLS      = "BOT TOOLS";
  private final static String ACTION_CHECK_WIKI     = "CHECK WIKI";
  private final static String ACTION_CURRENT_LIST   = "CURRENT LIST";
  private final static String ACTION_DEMO           = "DEMO";
  private final static String ACTION_DISAMBIGUATION = "DISAMBIGUATION";
  private final static String ACTION_FULL_ANALYSIS  = "FULL ANALYSIS";
  private final static String ACTION_HELP           = "HELP";
  private final static String ACTION_HELP_REQUESTED = "HELP_REQUESTED";
  private final static String ACTION_IDEA           = "IDEA";
  private final static String ACTION_LOGIN          = "LOGIN";
  private final static String ACTION_LOGOUT         = "LOGOUT";
  private final static String ACTION_OPTIONS        = "OPTIONS";
  private final static String ACTION_RANDOM_PAGE    = "RANDOM PAGE";
  private final static String ACTION_SAVE_PASSWORD  = "SAVE PASSWORD";
  private final static String ACTION_WATCHED_PAGES  = "WATCHED PAGES";

  public final static Integer WINDOW_VERSION = Integer.valueOf(3);

  private JComboBox comboWikipedia;
  private JComboBox comboLanguage;
  private JTextField textUsername;
  private JPasswordField textPassword;
  private char echoPassword = '*';
  private JCheckBox chckSavePassword;
  private JButton buttonLogin;
  private JButton buttonDemo;
  private JButton buttonLogout;
  private JButton buttonHelp;

  JTextField textPagename;
  private JButton buttonCheckWiki;
  private JTextField textCheckWikiErrors;
  private JButton buttonCurrentList;
  private JButton buttonDisambiguation;
  private JButton buttonFullAnalysis;
  private JButton buttonHelpRequested;
  private JButton buttonRandomPage;
  private JButton buttonWatchedPages;
  private JButton buttonBotTools;
  private JButton buttonOptions;
  private JButton buttonIdea;
  private JButton buttonAbout;

  boolean logged = false;

  /**
   * Create and display a MainWindow.
   */
  public static void createMainWindow() {
    createWindow(
        "MainWindow",
        null,
        JFrame.EXIT_ON_CLOSE,
        MainWindow.class,
        null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._(
        "Wikipedia Cleaner - Version {0} ({1})",
        new Object[] { Version.VERSION, DateFormat.getDateInstance().format(Version.DATE) });
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
    panel.add(createWorkComponents(), constraints);
    constraints.gridy++;

    // Message components
    if ((Version.MESSAGE != null) && !Version.MESSAGE.equals("")) {
      constraints.gridwidth = 2;
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
    EnumWikipedia wikipedia = getWikipedia();
    //Configuration config = Configuration.getConfiguration();

    comboWikipedia.setEnabled(!logged);
    comboLanguage.setEnabled(!logged);
    textUsername.setEnabled(!logged);
    textPassword.setEnabled(!logged);
    textPassword.setEchoChar(logged ? ' ' : echoPassword);
    buttonLogin.setEnabled(!logged);
    buttonDemo.setEnabled(!logged);
    buttonLogout.setEnabled(logged);

    textPagename.setEnabled(logged);
    buttonFullAnalysis.setEnabled(logged);
    buttonDisambiguation.setEnabled(logged);
    buttonCurrentList.setEnabled(logged);
    buttonHelpRequested.setEnabled(logged &&
                                   (wikipedia != null) &&
                                   (wikipedia.getTemplatesForHelpRequested() != null));
    buttonCheckWiki.setEnabled(logged && (wikipedia != null));
    textCheckWikiErrors.setEnabled(logged && (wikipedia != null));
    buttonRandomPage.setEnabled(logged);
    buttonWatchedPages.setEnabled(logged);
    buttonBotTools.setEnabled(logged);
  }

  /**
   * @return Message components.
   */
  private Component createMessageComponents() {
    JPanel panel = new JPanel(new GridLayout(1, 0));
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Message")));
    JLabel labelMessage = Utilities.createJLabel(Version.MESSAGE);
    labelMessage.setHorizontalAlignment(SwingConstants.LEADING);
    if (Version.HIGHLIGHT) {
      labelMessage.setForeground(Color.RED);
    }
    panel.add(labelMessage);
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
    constraints.insets = new Insets(2, 2, 2, 2);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Wikipedia
    comboWikipedia = new JComboBox(EnumWikipedia.getVector());
    comboWikipedia.addItem(GT._("Other Wikipedia"));
    comboWikipedia.setEditable(false);
    comboWikipedia.setSelectedItem(configuration.getWikipedia());
    JLabel labelWikipedia = Utilities.createJLabel(GT._("&Wikipedia"));
    labelWikipedia.setLabelFor(comboWikipedia);
    labelWikipedia.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelWikipedia, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(comboWikipedia, constraints);
    constraints.gridy++;

    // Language
    comboLanguage = new JComboBox(EnumLanguage.getVector());
    comboLanguage.addItem(GT._("Other Language"));
    comboLanguage.setEditable(false);
    comboLanguage.setSelectedItem(configuration.getLanguage());
    comboLanguage.addItemListener(this);
    JLabel labelLanguage = Utilities.createJLabel(GT._("Lan&guage"));
    labelLanguage.setLabelFor(comboLanguage);
    labelLanguage.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelLanguage, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(comboLanguage, constraints);
    constraints.gridy++;

    // User name
    textUsername = new JTextField(20);
    textUsername.setText(configuration.getString(Configuration.STRING_USER_NAME));
    JLabel labelUsername = Utilities.createJLabel(GT._("&User name :"));
    labelUsername.setLabelFor(textUsername);
    labelUsername.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelUsername, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(textUsername, constraints);
    constraints.gridy++;

    // Password
    textPassword = new JPasswordField(20);
    textPassword.setText(configuration.getString(Configuration.STRING_PASSWORD, null));
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

    chckSavePassword = Utilities.createJCheckBox(
        GT._("&Save password"),
        configuration.getString(Configuration.STRING_PASSWORD, null) != null);
    chckSavePassword.setActionCommand(ACTION_SAVE_PASSWORD);
    chckSavePassword.addActionListener(this);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.gridwidth = 2;
    panel.add(chckSavePassword, constraints);
    constraints.gridy++;

    // Login/Demo/Logout buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    buttonLogin = Utilities.createJButton(GT._("&Login"));
    buttonLogin.setActionCommand(ACTION_LOGIN);
    buttonLogin.addActionListener(this);
    buttonPanel.add(buttonLogin);
    buttonDemo = Utilities.createJButton(GT._("&Demo"));
    buttonDemo.setActionCommand(ACTION_DEMO);
    buttonDemo.addActionListener(this);
    buttonPanel.add(buttonDemo);
    buttonLogout = Utilities.createJButton(GT._("L&ogout"));
    buttonLogout.setActionCommand(ACTION_LOGOUT);
    buttonLogout.addActionListener(this);
    buttonPanel.add(buttonLogout);
    buttonHelp = Utilities.createJButton(GT._("&Help"));
    buttonHelp.setActionCommand(ACTION_HELP);
    buttonHelp.addActionListener(this);
    buttonPanel.add(buttonHelp);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    // Buttons
    buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    buttonOptions = Utilities.createJButton(GT._("&Options"));
    buttonOptions.setActionCommand(ACTION_OPTIONS);
    buttonOptions.addActionListener(this);
    buttonPanel.add(buttonOptions);
    buttonIdea = Utilities.createJButton(GT._("&Idea ? Bug ?"));
    buttonIdea.setActionCommand(ACTION_IDEA);
    buttonIdea.addActionListener(this);
    buttonPanel.add(buttonIdea);
    buttonAbout = Utilities.createJButton(GT._("About"));
    buttonAbout.setActionCommand(ACTION_ABOUT);
    buttonAbout.addActionListener(this);
    buttonAbout.setEnabled(false);
    buttonPanel.add(buttonAbout);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
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
    constraints.insets = new Insets(1, 1, 1, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Page name
    textPagename = new JTextField(configuration.getString(Configuration.STRING_PAGE_NAME), 20);
    panel.add(textPagename, constraints);
    constraints.gridy++;

    // Full analysis button
    buttonFullAnalysis = Utilities.createJButton(GT._("&Full analysis"));
    buttonFullAnalysis.setActionCommand(ACTION_FULL_ANALYSIS);
    buttonFullAnalysis.addActionListener(this);
    panel.add(buttonFullAnalysis, constraints);
    constraints.gridy++;

    // Disambiguation button
    buttonDisambiguation = Utilities.createJButton(GT._("&Disambiguation"));
    buttonDisambiguation.setActionCommand(ACTION_DISAMBIGUATION);
    buttonDisambiguation.addActionListener(this);
    panel.add(buttonDisambiguation, constraints);
    constraints.gridy++;

    // Random page button
    buttonRandomPage = Utilities.createJButton(GT._("&Random page"));
    buttonRandomPage.setActionCommand(ACTION_RANDOM_PAGE);
    buttonRandomPage.addActionListener(this);
    panel.add(buttonRandomPage, constraints);
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
    constraints.insets = new Insets(1, 1, 1, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Current list button
    buttonCurrentList = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("&Current Disambiguation List"), true);
    buttonCurrentList.setActionCommand(ACTION_CURRENT_LIST);
    buttonCurrentList.addActionListener(this);
    panel.add(buttonCurrentList, constraints);
    constraints.gridy++;

    // Check Wiki Project button
    buttonCheckWiki = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Project check wikipedia"), true);
    buttonCheckWiki.setActionCommand(ACTION_CHECK_WIKI);
    buttonCheckWiki.addActionListener(this);
    panel.add(buttonCheckWiki, constraints);
    constraints.gridy++;

    textCheckWikiErrors = new JTextField();
    textCheckWikiErrors.setToolTipText(GT._(
        "Comma separated list of errors to work on."));
    panel.add(textCheckWikiErrors, constraints);
    constraints.gridy++;

    // Help requested button
    buttonHelpRequested = Utilities.createJButton(GT._("Help requested on..."));
    buttonHelpRequested.setActionCommand(ACTION_HELP_REQUESTED);
    buttonHelpRequested.addActionListener(this);
    panel.add(buttonHelpRequested, constraints);
    constraints.gridy++;

    // Watched pages button
    buttonWatchedPages = Utilities.createJButton(GT._("Local &Watch list"));
    buttonWatchedPages.setActionCommand(ACTION_WATCHED_PAGES);
    buttonWatchedPages.addActionListener(this);
    panel.add(buttonWatchedPages, constraints);
    constraints.gridy++;

    // Bot tools button
    buttonBotTools = Utilities.createJButton(GT._("Bot tools"));
    buttonBotTools.setActionCommand(ACTION_BOT_TOOLS);
    buttonBotTools.addActionListener(this);
    /*panel.add(buttonBotTools, constraints);
    constraints.gridy++;*/

    return panel;
  }

  /**
   * @return Work components.
   */
  private Component createWorkComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Projects components
    panel.add(createProjectsComponents(), constraints);
    constraints.gridy++;

    // Page components
    panel.add(createPageComponents(), constraints);
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

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
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
    } else if (ACTION_CURRENT_LIST.equals(e.getActionCommand())) {
      actionCurrentList();
    } else if (ACTION_RANDOM_PAGE.equals(e.getActionCommand())) {
      actionRandomPage();
    } else if (ACTION_WATCHED_PAGES.equals(e.getActionCommand())) {
      actionWatchedPages();
    } else if (ACTION_OPTIONS.equals(e.getActionCommand())) {
      Controller.runOptions();
    } else if (ACTION_HELP_REQUESTED.equals(e.getActionCommand())) {
      actionHelpRequestedOn();
    } else if (ACTION_CHECK_WIKI.equals(e.getActionCommand())) {
      actionCheckWiki();
    } else if (ACTION_BOT_TOOLS.equals(e.getActionCommand())) {
      actionBotTools();
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
    if (comboWikipedia.getSelectedItem() instanceof String) {
      String url = "http://fr.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner#Other_Wikipedia";
      if (Utilities.isDesktopSupported()) {
        Utilities.browseURL(url);
      } else {
        // TODO: Let users copy / paste the url
        displayInformationMessage(
                "<html>" +
                GT._("You can learn how to add other Wikipedia at the following URL:") +
                "<br><a href=\"" + url + "\">" + url + "</a>" +
                "</html>");
      }
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
    if (comboLanguage.getSelectedItem() instanceof String) {
      String url = "http://fr.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner#Other_Language";
      if (Utilities.isDesktopSupported()) {
        Utilities.browseURL(url);
      } else {
        // TODO: Let users copy / paste the url
        displayInformationMessage(
                "<html>" +
                GT._("You can learn how to add other languages at the following URL:") +
                "<br><a href=\"" + url + "\">" + url + "</a>" +
                "</html>");
      }
      return;
    }
    EnumLanguage language = (EnumLanguage) comboLanguage.getSelectedItem();
    GT.setCurrentLanguage(language);

    // Check that correct values are entered for user name
    if (login) {
      if ((textUsername == null) ||
          (textUsername.getText() == null) ||
          ("".equals(textUsername.getText().trim()))) {
        displayWarning(
            GT._("You must input your user name before login"),
            textUsername);
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
        textUsername.getText(),
        textPassword.getPassword(),
        chckSavePassword.isSelected(),
        login).start();
  }

  /**
   * Action called when Logout button is pressed.
   */
  private void actionLogout() {
    API api = APIFactory.getAPI();
    api.logout();
    logged = false;
    updateComponentState();
  }

  /**
   * Action called when Help button is pressed.
   */
  private void actionHelp() {
    EnumWikipedia wikipedia = getWikipedia();
    String url = EnumWikipedia.FR.getHelpURL();
    if ((wikipedia != null) && (wikipedia.getHelpURL() != null)) {
      url = wikipedia.getHelpURL();
    }
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      // TODO: Let users copy / paste the url
      displayInformationMessage(
              "<html>" +
              GT._("You can read the help on Wikipedia Cleaner at the following URL:") +
              "<br><a href=\"" + url + "\">" + url + "</a>" +
              "</html>");
    }
  }

  /**
   * Action called when Idea button is pressed.
   */
  private void actionIdea() {
    String url = "http://fr.wikipedia.org/wiki/Discussion_Utilisateur:NicoV/Wikipedia_Cleaner";
    if (Utilities.isDesktopSupported()) {
      Utilities.browseURL(url);
    } else {
      // TODO: Let users copy / paste the url
      displayInformationMessage(
              "<html>" +
              GT._("You can submit bug reports or feature requests at the following URL:") +
              "<br><a href=\"" + url + "\">" + url + "</a>" +
              "</html>");
    }
  }

  /**
   * Action called when Save Password is changed. 
   */
  private void actionSavePassword() {
    if ((chckSavePassword.isSelected())) {
      int answer = displayYesNoWarning(
          GT._("The password will be saved on your disk, " +
               "so anyone having access to your computer may be able to get it.\n" +
               "Are you sure that you want to save it ?"));
      if (answer != JOptionPane.YES_OPTION) {
        chckSavePassword.setSelected(false);
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
    config.setString(Configuration.STRING_PAGE_NAME, textPagename.getText().trim());
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
    config.setString(Configuration.STRING_PAGE_NAME, textPagename.getText().trim());
    config.save();
    Controller.runDisambiguationAnalysis(
        textPagename.getText().trim(),
        getWikipedia());
  }

  /**
   * Action called when Current Disambiguation List is pressed.
   */
  private void actionCurrentList() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    if ((wikipedia.getDisambiguationList() == null) ||
        ("".equals(wikipedia.getDisambiguationList()))) {
      displayWarning(
          GT._("There's no known list of disambiguation pages for this Wikipedia."));
      return;
    }
    new DisambiguationListWorker(wikipedia, this).start();
  }

  /**
   * Action called when Help Requested On is pressed.
   */
  private void actionHelpRequestedOn() {
    EnumWikipedia wikipedia = getWikipedia();
    new EmbeddedInWorker(wikipedia, this, wikipedia.getTemplatesForHelpRequested()).start();
  }

  /**
   * Action called when Check Wiki is pressed. 
   */
  private void actionCheckWiki() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia == null) {
      return;
    }
    if (wikipedia.getCheckWikiProject() == null) {
      displayWarning(
          GT._("There's no known Check Wikipedia Project for this Wikipedia."));
      return;
    }
    Controller.runCheckWikiProject(getWikipedia(), textCheckWikiErrors.getText());
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
   * Action called when Watched pages button is pressed.
   */
  private void actionWatchedPages() {
    Configuration config = Configuration.getConfiguration();
    ArrayList<String> pageNames = config.getStringArrayList(Configuration.ARRAY_WATCH_PAGES);
    ArrayList<Page> pages = new ArrayList<Page>(
        (pageNames != null) ? pageNames.size() : 0);
    if (pageNames != null) {
      for (String name : pageNames) {
        pages.add(DataManager.getPage(getWikipedia(), name, null, null));
      }
    }
    PageListWindow.createPageListWindow(
        GT._("Watched pages"), pages,
        getWikipedia(), true);
  }

  /**
   * SwingWorker for login. 
   */
  class LoginWorker extends BasicWorker {

    private final EnumLanguage language;
    private final String username;
    private final char[] password;
    private final boolean savePassword;
    private final boolean login;

    public LoginWorker(
        EnumWikipedia wikipedia,
        BasicWindow window,
        EnumLanguage language,
        String username,
        char[] password,
        boolean savePassword,
        boolean login) {
      super(wikipedia, window);
      this.language = language;
      this.username = username.trim();
      this.password = password;
      this.savePassword = savePassword;
      this.login = login;
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
        setText(GT._("Login"));
        LoginResult result = api.login(getWikipedia(), username, new String(password), login);
        if (login) {
          if ((result == null) || (!result.isLoginSuccessful())) {
            throw new APIException("Login unsuccessful: " + ((result != null) ? result.toString() : ""));
          }
        }
        logged = true;

        // Saving settings
        Configuration configuration = Configuration.getConfiguration();
        configuration.setWikipedia(getWikipedia());
        configuration.setLanguage(language);
        if (login) {
          configuration.setString(Configuration.STRING_USER_NAME, username);
          configuration.setString(Configuration.STRING_PASSWORD , savePassword ? password : null);
        }
        Configuration.getConfiguration().save();

        // Retrieving disambiguation templates
        setText(GT._("Retrieving disambiguation templates"));
        getWikipedia().initDisambiguationTemplates(api);
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

    // New language selected: change default language
    if (e.getSource() == comboLanguage) {
      if (comboLanguage.getSelectedItem() instanceof EnumLanguage) {
        EnumLanguage language = (EnumLanguage) comboLanguage.getSelectedItem();
        GT.setCurrentLanguage(language);
        return;
      }
    }
  }
}
