/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.security.cert.CertificateException;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.Installer;
import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.simple.HelpButton;
import org.wikipediacleaner.gui.swing.component.simple.IdeaButton;
import org.wikipediacleaner.gui.swing.component.simple.LanguageSelector;
import org.wikipediacleaner.gui.swing.component.simple.PasswordInput;
import org.wikipediacleaner.gui.swing.component.simple.UserNameSelector;
import org.wikipediacleaner.gui.swing.component.simple.WikiChangeListener;
import org.wikipediacleaner.gui.swing.component.simple.WikiSelector;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;

import mslinks.ShellLink;

/**
 * Installer Window for WPCleaner. 
 */
public class InstallerWindow
  extends BasicWindow
  implements ActionListener, WikiChangeListener {

  /** Logger */
  static final Logger log = LoggerFactory.getLogger(Installer.class);

  public static final Integer WINDOW_VERSION = Integer.valueOf(1);

  /** Text field for the base directory for installation */
  private JTextField textBaseDirectory;

  /** Component for selecting the wiki */
  private WikiSelector wikiSelector;

  /** Component for selecting the language */
  private LanguageSelector languageSelector;

  /** Component for selecting the user */
  private UserNameSelector userNameSelector;

  /** Component for password input */
  private PasswordInput passwordInput;

  /** Component for creating a desktop shortcut */
  private JCheckBox chkDesktop;

  /** Component for selecting beta version */
  private JCheckBox chkBeta;

  /** Component for displaying explanations */
  private JTextArea textExplanations;

  /**
   * Create and display a window for the installer.
   */
  public static void createInstallerWindow() {
    createWindow(
        "InstallerWindow",
        null,
        JFrame.EXIT_ON_CLOSE,
        InstallerWindow.class,
        new InstallerWindowListener());
  }

  /**
   * @return Window title.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("WPCleaner installer");
  }

  /**
   * @return Currently selected wiki.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getWiki()
   */
  @Override
  public EnumWikipedia getWiki() {
    if (wikiSelector != null) {
      return wikiSelector.getWiki();
    }
    return super.getWiki();
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
    panel.add(createConfigurationComponents(), constraints);
    constraints.gridy++;
    constraints.weighty = 0;
    panel.add(createSystemInfoComponents(), constraints);
    constraints.gridy++;
    constraints.weighty = 0;
    panel.add(createCommandComponents(), constraints);
    constraints.gridy++;

    updateComponentState();
    wikiSelector.notifyWikiChange();
    return panel;
  }

  /**
   * @return Configuration components.
   */
  private Component createConfigurationComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("Configuration")));

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

    // Base directory
    File defaultBaseDirectory = new File(SystemUtils.getUserHome(), Version.PROGRAM);
    textBaseDirectory = Utilities.createJTextField(defaultBaseDirectory.getAbsolutePath(), 60);
    textBaseDirectory.getDocument().addDocumentListener(new DocumentListener() {

      /**
       * Gives notification that a portion of the document has been
       * removed.  The range is given in terms of what the view last
       * saw (that is, before updating sticky positions).
       *
       * @param e the document event
       * @see DocumentListener#removeUpdate(DocumentEvent)
       */
      @Override
      public void removeUpdate(DocumentEvent e) {
        changeText();
      }

      /**
       * Gives notification that there was an insert into the document.  The
       * range given by the DocumentEvent bounds the freshly inserted region.
       *
       * @param e the document event
       * @see DocumentListener#insertUpdate(DocumentEvent)
       */
      @Override
      public void insertUpdate(DocumentEvent e) {
        changeText();
      }

      /**
       * Gives notification that an attribute or set of attributes changed.
       *
       * @param e the document event
       * @see DocumentListener#changeUpdate(DocumentEvent)
       */
      @Override
      public void changedUpdate(DocumentEvent e) {
        changeText();
      }

      /**
       * Called for each modification of the text.
       */
      private void changeText() {
        updateExplanations();
      }
    });
    JLabel labelBaseDirectory = Utilities.createJLabel(GT._T("Installation folder"));
    labelBaseDirectory.setLabelFor(textBaseDirectory);
    labelBaseDirectory.setHorizontalAlignment(SwingConstants.TRAILING);
    JToolBar toolbarBaseDirectory = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarBaseDirectory.setFloatable(false);
    toolbarBaseDirectory.setBorderPainted(false);
    JButton buttonBaseDirectory = Utilities.createJButton(
        "gnome-folder.png", EnumImageSize.SMALL,
        GT._T("Select installation folder"), false, null);
    buttonBaseDirectory.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionBaseDirectory"));
    toolbarBaseDirectory.add(buttonBaseDirectory);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelBaseDirectory, constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(textBaseDirectory, constraints);
    constraints.gridx = 2;
    constraints.weightx = 0;
    panel.add(toolbarBaseDirectory, constraints);
    constraints.gridy++;

    // Wiki
    wikiSelector = new WikiSelector(getParentComponent());
    wikiSelector.addChangeListener(this);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(wikiSelector.getLabel(), constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(wikiSelector.getSelector(), constraints);
    constraints.gridx = 2;
    constraints.weightx = 0;
    panel.add(wikiSelector.getTools(), constraints);
    constraints.gridy++;

    // Language
    languageSelector = new LanguageSelector(getParentComponent());
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(languageSelector.getLabel(), constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(languageSelector.getSelector(), constraints);
    constraints.gridx = 2;
    constraints.weightx = 0;
    panel.add(languageSelector.getTools(), constraints);
    constraints.gridy++;

    // User
    userNameSelector = new UserNameSelector(getParentComponent());
    wikiSelector.addChangeListener(userNameSelector);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(userNameSelector.getLabel(), constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(userNameSelector.getSelector(), constraints);
    constraints.gridy++;

    // Password
    passwordInput = new PasswordInput(getParentComponent(), wikiSelector);
    userNameSelector.addChangeListener(passwordInput);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(passwordInput.getLabel(), constraints);
    constraints.gridx = 1;
    constraints.weightx = 1;
    panel.add(passwordInput.getField(), constraints);
    constraints.gridy++;

    // Selector for desktop shortcut
    chkDesktop = Utilities.createJCheckBox(GT._T("Create a desktop shortcut"), true);
    constraints.gridx = 0;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    panel.add(chkDesktop, constraints);
    constraints.gridy++;

    // Selector for beta version
    chkBeta = Utilities.createJCheckBox(GT._T("Install beta version (only for experienced users)"), false);
    constraints.gridx = 0;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    panel.add(chkBeta, constraints);
    constraints.gridy++;

    // Text area for explanations
    textExplanations = new JTextArea(getExplanations());
    JScrollPane scrollExplanations = new JScrollPane(
        textExplanations,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
    scrollExplanations.setMinimumSize(new Dimension(100, 100));
    constraints.gridx = 0;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(scrollExplanations, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Update explanations.
   */
  public void updateExplanations() {
    if (textExplanations != null) {
      textExplanations.setText(getExplanations());
    }
  }

  /**
   * @return Explanations about the installation.
   */
  private String getExplanations() {
    String directory = textBaseDirectory.getText();
    if ((directory == null) || (directory.trim().length() == 0)) {
      directory = SystemUtils.getUserHome().getAbsolutePath();
    }
    StringBuilder buffer = new StringBuilder();
    buffer.append(GT._T("A desktop shortcut can be created automatically under Windows and some Linux flavors."));
    buffer.append("\n\n");

    buffer.append(GT._T("If the desktop shortcut is not created automatically, you can still create it manually."));
    buffer.append("\n");
    buffer.append(GT._T("The working directory for the shortcut MUST be the installation folder ({0}).", directory));
    buffer.append("\n");
    buffer.append(GT._T("The command itself is dependent on the operating system you're using (see below)."));
    buffer.append("\n");
    buffer.append(GT._T("Note: avoid having whitespace characters in the installation folder path, or you will need to adapt the commands."));
    buffer.append("\n\n");

    buffer.append(GT._T("If you want to run WPCleaner manually, open a terminal/command prompt/... and change the current directory to the installation folder."));
    buffer.append("\n");
    buffer.append("cd ");
    buffer.append(directory);
    buffer.append("\n");
    buffer.append(GT._T("And then execute the command dependent on the operating system you're using (see below)."));
    buffer.append("\n\n");

    buffer.append(GT._T("Under Windows, the command can be:"));
    buffer.append("\n");
    buffer.append("WPCleaner.bat -wiki ");
    buffer.append(getWiki().getSettings().getCode());
    buffer.append("\n\n");

    buffer.append(GT._T("Under Linux (or other systems working with .sh scripts), the command can be:"));
    buffer.append("\n");
    buffer.append("WPCleaner.sh -wiki ");
    buffer.append(getWiki().getSettings().getCode());
    buffer.append("\n\n");

    buffer.append(GT._T("On any operating system, the command can be:"));
    buffer.append("\n");
    buffer.append("java -jar getdown.jar . client -credentials credentials.txt -wiki ");
    buffer.append(getWiki().getSettings().getCode());
    buffer.append("\n\n");

    return buffer.toString();
  }

  /**
   * @return Command components.
   */
  private Component createCommandComponents() {

    JPanel panel = new JPanel(new BorderLayout());

    // Installation button
    JButton buttonInstall = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.VERY_BIG,
        GT._T("Install"), true, null);
    buttonInstall.setFont(buttonInstall.getFont().deriveFont((float) 24));
    buttonInstall.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionInstall"));
    panel.add(buttonInstall, BorderLayout.CENTER);

    // Tool bar for buttons
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    toolbar.setBorderPainted(false);
    panel.add(toolbar, BorderLayout.LINE_END);

    // Help button
    HelpButton help = new HelpButton(getParentComponent(), wikiSelector);
    toolbar.add(help.getButton());

    // Idea button
    IdeaButton idea = new IdeaButton(getParentComponent());
    toolbar.add(idea.getButton());

    return panel;
  }

  /**
   * @return System information components.
   */
  private Component createSystemInfoComponents() {
    // Retrieve information about operating system
    StringBuilder sysInfo = new StringBuilder();
    sysInfo.append(GT._T("Operating system"));
    if (StringUtils.isNotBlank(SystemUtils.OS_NAME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.OS_NAME);
    }
    if (StringUtils.isNotBlank(SystemUtils.OS_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.OS_VERSION);
    }
    if (StringUtils.isNotBlank(SystemUtils.OS_ARCH)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.OS_ARCH);
    }

    // Retrieve information about Java
    sysInfo.append('\n');
    sysInfo.append("Java");
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VENDOR)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VENDOR);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VERSION);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_CLASS_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_CLASS_VERSION);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_COMPILER)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_COMPILER);
    }
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_VENDOR_URL)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VENDOR_URL);
    }*/

    // Retrieve information about Java Virtual Machine
    sysInfo.append('\n');
    sysInfo.append("Java VM");
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_NAME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_NAME);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_VERSION);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_VENDOR)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_VENDOR);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_INFO)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_INFO);
    }

    // Retrieve information about Java installation
    if (StringUtils.isNotBlank(SystemUtils.JAVA_HOME)) {
      sysInfo.append('\n');
      sysInfo.append("Java home=");
      sysInfo.append(SystemUtils.JAVA_HOME);
    }
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_CLASS_PATH)) {
      sysInfo.append('\n');
      sysInfo.append("Java class path=");
      sysInfo.append(SystemUtils.JAVA_CLASS_PATH);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_LIBRARY_PATH)) {
      sysInfo.append('\n');
      sysInfo.append("Java library path=");
      sysInfo.append(SystemUtils.JAVA_LIBRARY_PATH);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_ENDORSED_DIRS)) {
      sysInfo.append('\n');
      sysInfo.append("Java endorsed dirs=");
      sysInfo.append(SystemUtils.JAVA_ENDORSED_DIRS);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_EXT_DIRS)) {
      sysInfo.append('\n');
      sysInfo.append("Java ext dirs=");
      sysInfo.append(SystemUtils.JAVA_EXT_DIRS);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_IO_TMPDIR)) {
      sysInfo.append('\n');
      sysInfo.append("Java IO temporary dir=");
      sysInfo.append(SystemUtils.JAVA_IO_TMPDIR);
    }*/

    // Retrieve information about Java runtime
    sysInfo.append('\n');
    sysInfo.append("Java runtime");
    if (StringUtils.isNotBlank(SystemUtils.JAVA_RUNTIME_NAME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_RUNTIME_NAME);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_RUNTIME_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_RUNTIME_VERSION);
    }

    // Retrieve information about Java specification
    /*sysInfo.append('\n');
    sysInfo.append("Java specification");
    if (StringUtils.isNotBlank(SystemUtils.JAVA_SPECIFICATION_NAME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_SPECIFICATION_NAME);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_SPECIFICATION_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_SPECIFICATION_VERSION);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_SPECIFICATION_VENDOR)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_SPECIFICATION_VENDOR);
    }*/

    // Retrieve information about Java VM specification
    /*sysInfo.append('\n');
    sysInfo.append("Java VM specification");
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_SPECIFICATION_NAME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_SPECIFICATION_NAME);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_SPECIFICATION_VERSION)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_SPECIFICATION_VERSION);
    }
    if (StringUtils.isNotBlank(SystemUtils.JAVA_VM_SPECIFICATION_VENDOR)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.JAVA_VM_SPECIFICATION_VENDOR);
    }*/

    // Retrieve various information about AWT
    /*if (StringUtils.isNotBlank(SystemUtils.AWT_TOOLKIT)) {
      sysInfo.append('\n');
      sysInfo.append("AWT toolkit:");
      sysInfo.append(SystemUtils.AWT_TOOLKIT);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_AWT_FONTS)) {
      sysInfo.append('\n');
      sysInfo.append("Java AWT fonts:");
      sysInfo.append(SystemUtils.JAVA_AWT_FONTS);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_AWT_GRAPHICSENV)) {
      sysInfo.append('\n');
      sysInfo.append("Java AWT graphics environment:");
      sysInfo.append(SystemUtils.JAVA_AWT_GRAPHICSENV);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_AWT_HEADLESS)) {
      sysInfo.append('\n');
      sysInfo.append("Java AWT headless:");
      sysInfo.append(SystemUtils.JAVA_AWT_HEADLESS);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_AWT_PRINTERJOB)) {
      sysInfo.append('\n');
      sysInfo.append("Java AWT printer job:");
      sysInfo.append(SystemUtils.JAVA_AWT_PRINTERJOB);
    }*/

    // Retrieve various information about user
    /*sysInfo.append('\n');
    sysInfo.append("User");
    if (StringUtils.isNotBlank(SystemUtils.USER_NAME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.USER_NAME);
    }
    if (StringUtils.isNotBlank(SystemUtils.USER_COUNTRY)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.USER_COUNTRY);
    }
    if (StringUtils.isNotBlank(SystemUtils.USER_LANGUAGE)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.USER_LANGUAGE);
    }
    if (StringUtils.isNotBlank(SystemUtils.USER_TIMEZONE)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.USER_TIMEZONE);
    }
    if (StringUtils.isNotBlank(SystemUtils.USER_DIR)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.USER_DIR);
    }
    if (StringUtils.isNotBlank(SystemUtils.USER_HOME)) {
      sysInfo.append(" - ");
      sysInfo.append(SystemUtils.USER_HOME);
    }*/

    // Retrieve various information
    /*if (StringUtils.isNotBlank(SystemUtils.FILE_ENCODING)) {
      sysInfo.append('\n');
      sysInfo.append("File encoding:");
      sysInfo.append(SystemUtils.FILE_ENCODING);
    }*/
    /*if (StringUtils.isNotBlank(SystemUtils.JAVA_UTIL_PREFS_PREFERENCES_FACTORY)) {
      sysInfo.append('\n');
      sysInfo.append("Java utils prefs preferences factoryu:");
      sysInfo.append(SystemUtils.JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
    }*/

    // Log the information
    log.info(sysInfo.toString());

    // Display the information
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("System information")));
    JTextArea textArea = new JTextArea(sysInfo.toString());
    textArea.setEditable(false);
    panel.add(textArea, BorderLayout.CENTER);
    return panel;
  }

  /**
   * Action called when Directory button is pressed.
   */
  public void actionBaseDirectory() {
    File selectedDir = SystemUtils.getUserHome();
    if (textBaseDirectory.getText() != null) {
      selectedDir = new File(textBaseDirectory.getText());
    }
    JFileChooser chooser = new JFileChooser();
    chooser.setCurrentDirectory(selectedDir);
    chooser.setDialogTitle(Version.PROGRAM);
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    chooser.setAcceptAllFileFilterUsed(false);
    if (chooser.showOpenDialog(getParentComponent()) == JFileChooser.APPROVE_OPTION) {
      selectedDir = chooser.getSelectedFile();
      if (!selectedDir.getName().toUpperCase().contains(Version.PROGRAM.toUpperCase())) {
        selectedDir = new File(selectedDir, Version.PROGRAM);
      }
      textBaseDirectory.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  /**
   * Action called when wiki is changed.
   * 
   * @param wiki Current wiki.
   * @see org.wikipediacleaner.gui.swing.component.simple.WikiChangeListener#changeWiki(org.wikipediacleaner.api.constants.EnumWikipedia)
   */
  @Override
  public void changeWiki(EnumWikipedia wiki) {
    updateExplanations();
  }

  /**
   * Action called when Install button is pressed.
   */
  public void actionInstall() {

    // Check installation directory
    String directoryName = textBaseDirectory.getText();
    if ((directoryName == null) || (directoryName.trim().length() == 0)) {
      displayWarning(
          GT._T("You must select an installation folder."),
          textBaseDirectory);
      return;
    }
    File directory = new File(directoryName);
    if (directory.exists() && !directory.isDirectory()) {
      displayWarning(
          GT._T("The path {0} already exists but is not a folder.", directoryName),
          textBaseDirectory);
      return;
    }
    if (directory.exists()) {
      String[] files = directory.list();
      if ((files != null) && (files.length > 0)) {
        int answer = displayYesNoWarning(
            GT._T("The folder {0} already exists, do you want to install WPCleaner there?", directoryName) +
            "\n" +
            GT._T("Existing files may be deleted by the installation process."));
        if (answer != JOptionPane.YES_OPTION) {
          return;
        }
      }
    } else {
      if (!directory.mkdirs()) {
        displayWarning(
            GT._T("Unable to create folder {0}", directoryName),
            textBaseDirectory);
      }
    }

    // Retrieve properties for the credentials
    Properties credentials = new Properties();
    credentials.setProperty("user", userNameSelector.getUserName());
    credentials.setProperty("password", new String(passwordInput.getPassword()));

    // Retrieve choice for desktop shortcut
    boolean desktop = chkDesktop.isSelected();
    if (desktop) {
      if (!SystemUtils.IS_OS_LINUX &&
          !SystemUtils.IS_OS_WINDOWS) {
        displayWarning(
            GT._T("Desktop shortcut is only supported for the following operating systems:") + "\n" +
            "* Linux" + "\n" +
            "* Windows");
      }
    }

    // Perform the installation
    boolean beta = chkBeta.isSelected();
    InstallerWorker worker = new InstallerWorker(
        getWiki(), this, directory, credentials, beta);
    worker.start();
  }


  /**
   * Worker for WPCleaner installation.
   */
  private static class InstallerWorker extends BasicWorker {

    /** Installation directory */
    private final File directory;

    /** Warning message in case of a problem */
    private String warningMessage;

    /** Credentials */
    private Properties credentials;

    /** Flag for selecting beta version */
    private boolean beta;

    /**
     * @param wiki Wiki.
     * @param window Window.
     */
    public InstallerWorker(
        EnumWikipedia wiki, BasicWindow window,
        File directory, Properties credentials,
        boolean beta) {
      super(wiki, window);
      this.directory = directory;
      this.credentials = credentials;
      this.beta = beta;
      
    }

    /**
     * Called on the event dispatching thread (not on the worker thread)
     * after the <code>construct</code> method has returned.
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
     */
    @Override
    public void finished() {
      super.finished();
      if (warningMessage != null) {
        Utilities.displayWarning(
            getWindow().getParentComponent(),
            warningMessage, null);
      } else {
        Utilities.displayInformationMessage(
            getWindow().getParentComponent(),
            GT._T("Installation was successful"));
      }
    }

    /** 
     * Compute the value to be returned by the <code>get</code> method. 
     * 
     * @return Result of the worker.
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
     */
    @Override
    public Object construct() {

      // Download files for installation
      final int CONNECTION_TIMEOUT = 60000;
      final int READ_TIMEOUT = 60000;
      try {
        log.info("Downloading getdown.txt");
        setText(GT._T("Downloading {0}", "getdown.txt"));
        String remoteFolder = beta ?
            "https://wpcleaner.toolforge.org/wpcleaner-test" :
            "https://wpcleaner.toolforge.org/wpcleaner";
        FileUtils.copyURLToFile(
            new URL(remoteFolder + "/getdown.txt"),
            new File (directory, "getdown.txt"),
            CONNECTION_TIMEOUT, READ_TIMEOUT);
        log.info("Downloaded getdown.txt");
      } catch (IOException e) {
        handleDownloadError(e, "getdown.txt");
        return Boolean.FALSE;
      }
      try {
        log.info("Downloading getdown.jar");
        setText(GT._T("Downloading {0}", "getdown.jar"));
        FileUtils.copyURLToFile(
            new URL("https://wpcleaner.toolforge.org/install/getdown.jar"),
            new File (directory, "getdown.jar"),
            CONNECTION_TIMEOUT, READ_TIMEOUT);
        log.info("Downloaded getdown.jar");
      } catch (IOException e) {
        handleDownloadError(e, "getdown.jar");
        return Boolean.FALSE;
      }

      // Create credentials.txt file
      if (credentials != null) {
        log.info("Creating credentials.txt");
        try (OutputStream os = new FileOutputStream(new File(directory, "credentials.txt"))) {
          setText(GT._T("Creating {0}", "credentials.txt"));
          credentials.store(
              os,
              "Credentials for automatic login of WPCleaner");
          log.info("Created credentials.txt");
        } catch (IOException e) {
          warningMessage =
              GT._T("Unable to create file {0}", "credentials.txt") + "\n" +
              GT._T("Error: {0}", e.getLocalizedMessage());
          log.error("Error while creating credentials.txt: " + e.getMessage());
          return Boolean.FALSE;
        }
      }

      // Install WPCleaner
      try {
        log.info("Installing");
        setText(GT._T("Installing {0}", Version.PROGRAM));
        ProcessBuilder pb = new ProcessBuilder(
            "java",
            "-Dsilent",
            "-Dthread_pool_size=1",
            "-jar", new File(directory, "getdown.jar").getAbsolutePath(),
            directory.getAbsolutePath());
        pb.directory(directory);
        log.info("Starting installation");
        Process process = pb.start();
        log.info("Installation started");
        int result = process.waitFor();
        log.info("Installation finished");
        if (result != 0) {
          warningMessage =
              GT._T("Problem running getdown installer") + "\n" +
              GT._T("Exit code: {0}", Integer.toString(result));
          log.error("Error running getdown installer (" + result + ")");
        }
      } catch (InterruptedException | IOException e) {
        warningMessage =
            GT._T("Problem running {0}", "getdown") + "\n" +
            GT._T("Error: {0}", e.getLocalizedMessage());
        log.error("Error running getdown installer: " + e.getMessage());
        return Boolean.FALSE;
      }

      // Create a desktop shortcut
      try {
        File desktopFile = createDesktopFile();

        // Linux
        if (SystemUtils.IS_OS_LINUX) {

          // Copy desktop file to normal location
          log.info("Copying desktop file to normal location");
          File normalDesktopFile = new File(
              new File(SystemUtils.getUserDir(), ".local/share/applications"),
              desktopFile.getName());
          FileUtils.copyFile(desktopFile, normalDesktopFile);
          log.info("Copied desktop file to normal location");

          // Copy desktop file to old GNOME location
          File applicationsFolder = new File(SystemUtils.getUserDir(), ".gnomev2/vfolders/applications");
          if (applicationsFolder.exists() && applicationsFolder.isDirectory()) {
            log.info("Copying desktop file to old GNOME location");
            FileUtils.copyFile(desktopFile, new File(applicationsFolder, desktopFile.getName()));
            log.info("Copyied desktop file to old GNOME location");
          }
        }

        // Windows
        if (SystemUtils.IS_OS_WINDOWS) {
          log.info("Creating shell link to WPCleaner.bat");
          ShellLink sl = ShellLink.createLink(new File(directory, "WPCleaner.bat").getAbsolutePath())
              .setWorkingDir(directory.getAbsolutePath())
              .setIconLocation(new File(directory, "WPCleaner.ico").getAbsolutePath())
              .setCMDArgs("-wiki " + getWikipedia().getSettings().getCode());
          File desktopFolder = new File(SystemUtils.USER_HOME, "Desktop");
          if (!desktopFolder.exists() || !desktopFolder.isDirectory()) {
            desktopFolder = directory;
          }
          String shortcutName = beta ? "WPCleaner - Test" : "WPCleaner";
          File shortcutFile = new File(desktopFolder, shortcutName + ".lnk");
          if (shortcutFile.exists()) {
            int counter = 1;
            do {
              shortcutFile = new File(desktopFolder, shortcutName + " (" + counter + ").lnk");
              counter++;
            } while (shortcutFile.exists());
          }
          sl.saveTo(shortcutFile.getAbsolutePath());
          log.info("Created shell link to WPCleaner.bat");
        }
      } catch (IOException e) {
        warningMessage =
            GT._T("Problem creating desktop file") + "\n" +
            GT._T("Error: {0}", e.getLocalizedMessage());
        log.error("Error creating shortcut: " + e.getMessage());
      }

      // Run WPCleaner
      try {
        log.info("Running");
        setText(GT._T("Running {0}", Version.PROGRAM));
        ProcessBuilder pb = new ProcessBuilder(
            "java",
            "-jar", new File(directory, "getdown.jar").getAbsolutePath(),
            directory.getAbsolutePath(),
            "client",
            "-credentials", new File(directory, "credentials.txt").getAbsolutePath(),
            "-wiki", getWikipedia().getSettings().getCode());
        pb.directory(directory);
        pb.start();
        log.info("Started");
      } catch (IOException e) {
        warningMessage =
            GT._T("Problem running {0}", Version.PROGRAM) + "\n" +
            GT._T("Error: {0}", e.getLocalizedMessage());
        log.error("Error running: " + e.getMessage());
        return Boolean.FALSE;
      }

      return Boolean.TRUE;
    }

    /**
     * Handle download error.
     * 
     * @param e Exception thrown by the download.
     * @param file File which failed to download.
     */
    private void handleDownloadError(IOException e, String file) {
      if (e == null) {
        return;
      }
      StringBuilder message = new StringBuilder();
      message.append(GT._T("Unable to download file {0}.", file));
      Throwable cause = e.getCause();
      if (cause instanceof CertificateException) {
        message.append("\n\n");
        message.append(GT._T("Certificate exception: old versions of Java may fail to validate recent certificates, try upgrading Java"));
        message.append("\n");
        message.append(GT._T("Java version: {}", System.getProperty("java.version")));
      }
      message.append("\n\n");
      message.append(GT._T("Error: {0}", e.getLocalizedMessage()));
      warningMessage = message.toString();
      log.error(warningMessage);
    }

    /**
     * Create a desktop file (see <a href="https://developer.gnome.org/integration-guide/stable/desktop-files.html.en">GNOME</a>).
     * 
     * @return Desktop file.
     * @throws IOException In case of a problem writing the desktop file.
     */
    private File createDesktopFile() throws IOException {
      log.info("Creating WPCleaner.desktop");
      File desktopFile = new File(directory, "WPCleaner.desktop");
      try (FileOutputStream fos = new FileOutputStream(desktopFile);
           OutputStreamWriter osw = new OutputStreamWriter(fos, "UTF8");
           BufferedWriter writer = new BufferedWriter(osw)) {
        writer.write("[Desktop Entry]\n");
        writer.write("Type=Application\n");
        writer.write("Version=1.1\n");
        writer.write("Name=" + (beta ? "WPCleaner Test" : "WPCleaner") + "\n");
        writer.write("Comment=Perform maintenance on Wikipedia\n");
        writer.write("Icon=" + new File(directory, "WPCleaner.png").getAbsolutePath() + "\n");
        writer.write("Exec=" + new File(directory, "WPCleaner.sh").getAbsolutePath() + " -wiki " + getWikipedia().getSettings().getCode() + "\n");
        writer.write("Path=" + directory.getAbsolutePath() + "\n");
        writer.write("Categories=Utility\n");
        writer.write("Keywords=Wikipedia\n");
        writer.write("StartupNotify=false\n");
      }
      log.info("Created WPCleaner.desktop");
      return desktopFile;
    }
  }

  /**
   * Listener for window events.
   */
  private static class InstallerWindowListener implements BasicWindowListener {

    /**
     * Constructor
     */
    public InstallerWindowListener() {
      // Nothing to do
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
      if (window == null) {
        return;
      }
      window.getParentComponent().setLocationRelativeTo(null);
    }
  }
}
