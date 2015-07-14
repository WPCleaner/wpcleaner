/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.checkwiki;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.beans.EventHandler;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeListener;

import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.CheckWikiListener;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmComparator;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.OnePageWindow;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.HTMLPane;
import org.wikipediacleaner.gui.swing.component.JCloseableTabbedPane;
import org.wikipediacleaner.gui.swing.worker.CheckWikiProjectWorker;
import org.wikipediacleaner.gui.swing.worker.RetrieveContentWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;

/**
 * Check Wiki Project window.
 */
public class CheckWikiWindow extends OnePageWindow implements CheckWikiListener {

  List<CheckErrorAlgorithm> allAlgorithms;
  private List<CheckErrorAlgorithm> selectedAlgorithms;
  private List<JMenuItem> menuItemAlgorithms;
  private JPopupMenu popupSelectErrors;
  private JMenu menuUseSelection;
  private JMenu menuDeleteSelection;
  private JButton buttonSelectErrors;
  private JButton buttonLoadErrors;

  private SpinnerNumberModel modelMaxErrors;

  List<CheckError> errors;
  Properties checkWikiConfig;
  JComboBox listAllErrors;
  DefaultComboBoxModel modelAllErrors;
  private HTMLPane textDescription;
  private HTMLPane textParameters;
  private int lastErrorDisplayed = -1;
  private JButton buttonReloadError;
  private JButton buttonErrorDetail;
  private JButton buttonErrorList;
  private JButton buttonWhiteList;

  private JList listPages;
  private DefaultListModel modelPages;
  boolean yesAll = false;
  boolean noAll = false;

  JTabbedPane contentPane;

  public final static String ACTION_SELECT_ERRORS = "SELECT_ERRORS:";

  /**
   * Create and display a CheckWikiProjectWindow.
   * 
   * @param wikipedia Wikipedia.
   */
  public static void createCheckWikiProjectWindow(
      final EnumWikipedia wikipedia) {
    createWindow(
        "CheckWikiWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        CheckWikiWindow.class,
        null);
  }

  /**
   * @return Window title.
   */
  @Override
  public String getTitle() {
    return GT._("Check Wikipedia");
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(Box.createHorizontalGlue());
    return menuBar;
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

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

    // Check Wikipedia Project
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(createProjectComponents(), constraints);
    constraints.gridy++;

    // Page list
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;
    panel.add(createPageListComponents(), constraints);

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx++;
    constraints.weightx = 1;
    constraints.weighty = 1;
    contentPane = new JCloseableTabbedPane();
    contentPane.setPreferredSize(new Dimension(900, 600));
    contentPane.addChangeListener(EventHandler.create(
        ChangeListener.class, this, "displayErrorDescription"));
    panel.add(contentPane, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * @return Message for the Load button.
   */
  private String getLoadMessage() {
    if ((allAlgorithms == null) || (allAlgorithms.isEmpty())) {
      return GT._("No errors available");
    }
    if ((selectedAlgorithms == null) || (selectedAlgorithms.isEmpty())) {
      return GT._("No errors selected");
    }

    // Check algorithms selected
    boolean allErrorsSelected = true;
    boolean allTopPrioritySelected = true;
    boolean noTopPrioritySelected = true;
    boolean allMiddlePrioritySelected = true;
    boolean noMiddlePrioritySelected = true;
    boolean allLowestPrioritySelected = true;
    boolean noLowestPrioritySelected = true;
    boolean allBotOnlyPrioritySelected = true;
    boolean noBotOnlyPrioritySelected = true;
    for (CheckErrorAlgorithm algorithm : allAlgorithms) {
      boolean useAlgorithm = false;
      if (algorithm.isAvailable() &&
          CWConfigurationError.isPriorityFullyActive(algorithm.getPriority())) {
        if (algorithm.hasList()) {
          useAlgorithm = true;
        }
      }
      if (useAlgorithm) {
        if (!selectedAlgorithms.contains(algorithm)) {
          allErrorsSelected = false;
          switch (algorithm.getPriority()) {
          case CWConfigurationError.PRIORITY_TOP:
            allTopPrioritySelected = false;
            break;
          case CWConfigurationError.PRIORITY_MIDDLE:
            allMiddlePrioritySelected = false;
            break;
          case CWConfigurationError.PRIORITY_LOWEST:
            allLowestPrioritySelected = false;
            break;
          case CWConfigurationError.PRIORITY_BOT_ONLY:
            allBotOnlyPrioritySelected = false;
            break;
          }
        } else {
          switch (algorithm.getPriority()) {
          case CWConfigurationError.PRIORITY_TOP:
            noTopPrioritySelected = false;
            break;
          case CWConfigurationError.PRIORITY_MIDDLE:
            noMiddlePrioritySelected = false;
            break;
          case CWConfigurationError.PRIORITY_LOWEST:
            noLowestPrioritySelected = false;
            break;
          case CWConfigurationError.PRIORITY_BOT_ONLY:
            noBotOnlyPrioritySelected = false;
            break;
          }
        }
      }
    }

    // Get message depending on algorithms selected
    if (allErrorsSelected) {
      return GT._("Load all errors");
    } else if (allTopPrioritySelected &&
               noMiddlePrioritySelected &&
               noLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all high priority errors");
    } else if (allTopPrioritySelected &&
               allMiddlePrioritySelected &&
               noLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all high and middle priority errors");
    } else if (noTopPrioritySelected &&
               allMiddlePrioritySelected &&
               noLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all middle priority errors");
    } else if (allTopPrioritySelected &&
               allMiddlePrioritySelected &&
               allLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all high, middle and lowest priority errors");
    } else if (allTopPrioritySelected &&
               noMiddlePrioritySelected &&
               allLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all high and lowest priority errors");
    } else if (noTopPrioritySelected &&
               allMiddlePrioritySelected &&
               allLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all middle and lowest priority errors");
    } else if (noTopPrioritySelected &&
               noMiddlePrioritySelected &&
               allLowestPrioritySelected &&
               noBotOnlyPrioritySelected) {
      return GT._("Load all lowest priority errors");
    } else if (noTopPrioritySelected &&
               noMiddlePrioritySelected &&
               noLowestPrioritySelected &&
               allBotOnlyPrioritySelected) {
      return GT._("Load all errors for bots");
    }

    // Message with an explicit list of errors
    StringBuilder msg = new StringBuilder();
    for (int i = 0; i < selectedAlgorithms.size(); i++) {
      int j = i;
      while ((j + 1 < selectedAlgorithms.size()) &&
             (selectedAlgorithms.get(j).getErrorNumber() + 1 == selectedAlgorithms.get(j + 1).getErrorNumber())) {
        j++;
      }
      if (msg.length() > 0) {
        msg.append(", ");
      }
      if (j > i + 1) {
        msg.append(selectedAlgorithms.get(i).getErrorNumber());
        msg.append("-");
        msg.append(selectedAlgorithms.get(j).getErrorNumber());
        i = j;
      } else {
        msg.append(selectedAlgorithms.get(i).getErrorNumber());
      }
    }
    return GT._("Load errors {0}", msg.toString());
  }

  /**
   * Create popup menu for selecting errors.
   */
  private void createPopupSelectErrors() {
    popupSelectErrors = new JPopupMenu(GT._("Select errors"));
    menuItemAlgorithms = new ArrayList<JMenuItem>();
    JMenuItem menuItem = null;

    menuItem = new JMenuItem(GT._("Select all errors"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "*");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);

    menuItem = new JMenuItem(GT._("Select high priority errors"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "P1");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);

    menuItem = new JMenuItem(GT._("Select middle priority (and above) errors"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "P1,P2");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);

    menuItem = new JMenuItem(GT._("Select middle priority errors"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "P2");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);

    menuItem = new JMenuItem(GT._("Select lowest priority (and above) errors"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "P1,P2,P3");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);

    menuItem = new JMenuItem(GT._("Select lowest priority errors"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "P3");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);

    popupSelectErrors.addSeparator();

    final Map<TextAttribute, Boolean> inactiveAttributes = new HashMap<TextAttribute, Boolean>();
    inactiveAttributes.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);

    final int PART_SIZE = 20; 
    int lastPart = -1;
    JMenu subMenu = null;
    for (CheckErrorAlgorithm algorithm : allAlgorithms) {
      if (algorithm != null) {
        int errorNumber = algorithm.getErrorNumber();
        boolean useAlgorithm = false;
        if (errorNumber > 0) {
          if (algorithm.hasList()) {
            useAlgorithm = true;
          }
        }
        if (useAlgorithm) {
          int part = (errorNumber - 1) / PART_SIZE;
          if ((subMenu == null) || (part > lastPart)) {
            int from = (part * PART_SIZE) + 1;
            int to = (part + 1) * PART_SIZE;
            subMenu = new JMenu(GT._(
                "Errors from {0} to {1}",
                new Object[] { Integer.valueOf(from), Integer.valueOf(to) }));
            popupSelectErrors.add(subMenu);

            lastPart = part;
          }
          String label =
            algorithm.getErrorNumberString() + " - " +
            algorithm.getShortDescriptionReplaced();

          menuItem = new JCheckBoxMenuItem(label, selectedAlgorithms.contains(algorithm));
          if (!CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
            menuItem.setEnabled(false);
            menuItem.setFont(menuItem.getFont().deriveFont(inactiveAttributes));
          } else if (!algorithm.isAvailable()) {
            menuItem.setEnabled(false);
          } else if (CWConfigurationError.PRIORITY_BOT_ONLY == algorithm.getPriority()) {
            menuItem.setEnabled(false);
            menuItem.setFont(menuItem.getFont().deriveFont(Font.ITALIC));
            menuItem.setSelected(false);
          }
          menuItem.setActionCommand(ACTION_SELECT_ERRORS + "+" + algorithm.getErrorNumberString());
          menuItem.addActionListener(this);
          subMenu.add(menuItem);

          while (menuItemAlgorithms.size() <= errorNumber) {
            menuItemAlgorithms.add(null);
          }
          menuItemAlgorithms.set(errorNumber, menuItem);
        }
      }
    }

    // Select only
    popupSelectErrors.addSeparator();
    lastPart = -1;
    subMenu = null;
    for (CheckErrorAlgorithm algorithm : allAlgorithms) {
      if (algorithm != null) {
        int errorNumber = algorithm.getErrorNumber();
        boolean useAlgorithm = false;
        if (errorNumber > 0) {
          if ((errorNumber <= CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) ||
              algorithm.hasSpecialList()) {
            useAlgorithm = true;
          }
        }
        if (useAlgorithm) {
          int part = (errorNumber - 1) / PART_SIZE;
          if ((subMenu == null) || (part > lastPart)) {
            int from = (part * PART_SIZE) + 1;
            int to = (part + 1) * PART_SIZE;
            subMenu = new JMenu(
                GT._("Select only") + " - " +
                GT._(
                    "Errors from {0} to {1}",
                    new Object[] { Integer.valueOf(from), Integer.valueOf(to) }));
            popupSelectErrors.add(subMenu);

            lastPart = part;
          }
          String label =
            algorithm.getErrorNumberString() + " - " +
            algorithm.getShortDescriptionReplaced();
          menuItem = new JMenuItem(label);
          if (!CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
            menuItem.setEnabled(false);
            menuItem.setFont(menuItem.getFont().deriveFont(inactiveAttributes));
          } else if (!algorithm.isAvailable()) {
            menuItem.setEnabled(false);
          } else if (CWConfigurationError.PRIORITY_BOT_ONLY == algorithm.getPriority()) {
            menuItem.setEnabled(false);
            menuItem.setFont(menuItem.getFont().deriveFont(Font.ITALIC));
          }
          menuItem.setActionCommand(ACTION_SELECT_ERRORS + algorithm.getErrorNumberString());
          menuItem.addActionListener(this);
          subMenu.add(menuItem);
        }
      }
    }

    // Saved selections
    popupSelectErrors.addSeparator();
    menuItem = new JMenuItem(GT._("Save current selection"));
    menuItem.setActionCommand(ACTION_SELECT_ERRORS + "S");
    menuItem.addActionListener(this);
    popupSelectErrors.add(menuItem);
    menuUseSelection = new JMenu(GT._("Use selection"));
    popupSelectErrors.add(menuUseSelection);
    menuDeleteSelection = new JMenu(GT._("Delete selection"));
    popupSelectErrors.add(menuDeleteSelection);
    updateSavedSelections();
  }

  /**
   * Update menus for the saved selections.
   */
  private void updateSavedSelections() {
    menuUseSelection.removeAll();
    menuDeleteSelection.removeAll();
    Configuration config = Configuration.getConfiguration();
    Properties properties = config.getProperties(null, Configuration.ARRAY_CHECK_SELECTION);
    Set<Object> keySet = properties.keySet();
    List<String> keyList = new ArrayList<String>();
    for (Object key : keySet) {
      keyList.add(key.toString());
    }
    Collections.sort(keyList);
    JMenuItem menuItem = null;
    for (String name : keyList) {
      String selection = properties.getProperty(name);
      menuItem = new JMenuItem(name + ": " + selection);
      menuItem.setActionCommand(ACTION_SELECT_ERRORS + selection);
      menuItem.addActionListener(this);
      menuUseSelection.add(menuItem);

      menuItem = new JMenuItem(name + ": " + selection);
      menuItem.setActionCommand(ACTION_SELECT_ERRORS + "D" + name);
      menuItem.addActionListener(this);
      menuDeleteSelection.add(menuItem);
    }
  }

  /**
   * Update popup menu for selecting errors. 
   */
  private void updatePopupSelectErrors() {
    for (CheckErrorAlgorithm algorithm : allAlgorithms) {
      if (algorithm != null) {
        int errorNumber = algorithm.getErrorNumber();
        if (errorNumber < menuItemAlgorithms.size()) {
          JMenuItem menuItem = menuItemAlgorithms.get(errorNumber);
          if (menuItem != null) {
            menuItem.setSelected(selectedAlgorithms.contains(algorithm));
          }
        }
      }
    }
  }

  /**
   * @return Project components
   */
  private Component createProjectComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
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

    // Initialize algorithms list
    allAlgorithms = CheckErrorAlgorithms.getAlgorithms(getWikipedia());
    if (allAlgorithms == null) {
      allAlgorithms = Collections.emptyList();
    }
    selectedAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    for (CheckErrorAlgorithm algorithm : allAlgorithms) {
      if (algorithm.isAvailable() &&
          CWConfigurationError.isPriorityFullyActive(algorithm.getPriority())) {
        selectedAlgorithms.add(algorithm);
      }
    }
    createPopupSelectErrors();

    // Loading
    JToolBar toolbarLoad = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarLoad.setFloatable(false);

    modelMaxErrors = new SpinnerNumberModel(
        configuration.getInt(
            null,
            ConfigurationValueInteger.CHECK_NB_ERRORS),
        10, 1000, 5);
    JSpinner spinMaxErrors = new JSpinner(modelMaxErrors);
    spinMaxErrors.setPreferredSize(new Dimension(80, 25));
    spinMaxErrors.setMaximumSize(new Dimension(80, 25));
    JLabel labelMaxErrors = Utilities.createJLabel(
        GT._("Maximum number of errors for Check Wiki :"));
    labelMaxErrors.setLabelFor(spinMaxErrors);
    labelMaxErrors.setHorizontalAlignment(SwingConstants.TRAILING);
    toolbarLoad.add(labelMaxErrors);
    toolbarLoad.add(spinMaxErrors);

    toolbarLoad.addSeparator();

    buttonSelectErrors = Utilities.createJButton(
        "gnome-preferences-desktop.png", EnumImageSize.NORMAL,
        GT._("Select errors"), true, null);
    buttonSelectErrors.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionChooseErrors"));
    toolbarLoad.add(buttonSelectErrors);
    buttonLoadErrors = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        getLoadMessage(), true, null);
    buttonLoadErrors.setActionCommand(ACTION_RELOAD);
    buttonLoadErrors.addActionListener(this);
    buttonLoadErrors.setPreferredSize(new Dimension(800, 20));
    buttonLoadErrors.setHorizontalAlignment(SwingConstants.LEADING);
    buttonLoadErrors.setComponentPopupMenu(popupSelectErrors);
    buttonLoadErrors.setToolTipText(GT._("Right click to select errors"));
    toolbarLoad.add(buttonLoadErrors);

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 3;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(toolbarLoad, constraints);
    constraints.gridy++;

    // List of errors managed by the project
    JLabel labelErrors = Utilities.createJLabel(GT._("List of errors detected :"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(labelErrors, constraints);
    modelAllErrors = new DefaultComboBoxModel();
    listAllErrors = new JComboBox(modelAllErrors);
    listAllErrors.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
        actionSelectErrorType();
      }
    });
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(listAllErrors, constraints);
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    buttonReloadError = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        GT._("Reload error"), false, null);
    buttonReloadError.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionReloadError"));
    toolbar.add(buttonReloadError);
    buttonErrorDetail = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.NORMAL,
        GT._("Detail"), false, null);
    buttonErrorDetail.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionErrorDetail"));
    buttonErrorDetail.setEnabled(false);
    toolbar.add(buttonErrorDetail);
    buttonErrorList = Utilities.createJButton(
        "gnome-web-browser.png", EnumImageSize.NORMAL,
        GT._("List on {0}", CheckWiki.getServerName(getWikipedia())), false, null);
    buttonErrorList.addActionListener(
        EventHandler.create(ActionListener.class, this, "actionErrorList"));
    buttonErrorList.setEnabled(false);
    toolbar.add(buttonErrorList);
    buttonWhiteList = Utilities.createJButton(
        "gnome-accessories-text-editor.png", EnumImageSize.NORMAL,
        GT._("View or edit whitelist"), false, null);
    buttonWhiteList.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionErrorWhiteList"));
    buttonWhiteList.setEnabled(false);
    toolbar.add(buttonWhiteList);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(toolbar, constraints);
    constraints.gridx = 0;
    constraints.gridy++;

    // Error description
    textDescription = HTMLPane.createHTMLPane(null);
    textDescription.setPreferredSize(new Dimension(500, 100));
    textDescription.setMinimumSize(new Dimension(200, 100));

    // Parameters description
    textParameters = HTMLPane.createHTMLPane(null);
    textParameters.setPreferredSize(new Dimension(500, 100));
    textParameters.setMinimumSize(new Dimension(200, 100));

    // Split pane
    JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    splitPane.setLeftComponent(textDescription);
    splitPane.setRightComponent(textParameters);
    splitPane.setPreferredSize(new Dimension(700, 100));
    splitPane.setMinimumSize(new Dimension(300, 100));
    splitPane.setResizeWeight(1.0);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 3;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(splitPane, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Page list components
   */
  private Component createPageListComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Pages")));

    modelPages = new DefaultListModel();
    listPages = new JList(modelPages);

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
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Load pages
    JToolBar toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    JButton buttonLoad = Utilities.createJButton(GT._("&Load pages"), null);
    buttonLoad.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLoadPages"));
    toolbarButtons.add(buttonLoad);
    ActionFullAnalysis.addButton(
        getParentComponent(), toolbarButtons, getWikipedia(), listPages, null, true, true);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(toolbarButtons, constraints);
    constraints.gridy++;

    // Page List
    listPages.setCellRenderer(new CheckErrorPageListCellRenderer(true));
    listPages.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    listPages.addMouseListener(new MouseAdapter() {

      /* (non-Javadoc)
       * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
       */
      @Override
      public void mouseClicked(MouseEvent e) {
        if (e.getButton() != MouseEvent.BUTTON1) {
          return;
        }
        if (e.getClickCount() != 2) {
          return;
        }
        actionLoadPages();
      }
    });
    JScrollPane scrollPages = new JScrollPane(listPages);
    scrollPages.setMinimumSize(new Dimension(200, 200));
    scrollPages.setPreferredSize(new Dimension(200, 300));
    scrollPages.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;
    panel.add(scrollPages, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @param pane Pane.
   * @param page Page.
   * @param pageErrors Errors detected in the page.
   * @return Contents components.
   */
  public CheckWikiContentPanel createContentsComponents(
      JTabbedPane pane, Page page,
      List<CheckError> pageErrors) {
    CheckWikiContentPanel panel = new CheckWikiContentPanel(
        this, pane, page, pageErrors);
    panel.initialize();
    return panel;
  }
  
  /**
   * Callback called at the end of the Reload Worker.
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    checkWiki.addListener(this);
    analyzeCheckWiki();
  }

  /**
   * Analyze the Check Wiki page contents.
   */
  private void analyzeCheckWiki() {
    //String contents = projectPage.getContents();
    //errors = CheckError.initCheckErrors(getWikipedia(), contents);
    if (modelAllErrors != null) {
      int selectedError = 0;
      if (listAllErrors.getSelectedItem() instanceof CheckError) {
        selectedError = ((CheckError) listAllErrors.getSelectedItem()).getErrorNumber();
      }
      modelAllErrors.removeAllElements();
      Configuration config = Configuration.getConfiguration();
      boolean showAllErrors = config.getBoolean(
          null,
          ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS);
      int selectedIndex = -1;
      if (errors != null) {
        for (CheckError error : errors) {
          if ((error.getPageCount() > 0) || (showAllErrors)) {
            if (error.getErrorNumber() == selectedError) {
              selectedIndex = modelAllErrors.getSize();
            }
            modelAllErrors.addElement(error);
          }
        }
        if (!getPagesWithSeveralErrors().isEmpty()) {
          modelAllErrors.insertElementAt(GT._("Pages with several errors"), 0);
          if (selectedIndex > 0) {
            selectedIndex++;
          }
        }
      }
      selectedIndex = Math.max(selectedIndex, 0);
      if (listAllErrors.getItemCount() > selectedIndex) {
        listAllErrors.setSelectedIndex(selectedIndex);
      }
    }
  }

  /**
   * @return List of pages with several errors.
   */
  private List<String> getPagesWithSeveralErrors() {
    Set<String> pagesWithMultipleErrors = new HashSet<String>();
    Set<String> pagesWithError = new HashSet<String>();
    for (CheckError error : errors) {
      for (int pageNumber = 0; pageNumber < error.getPageCount(); pageNumber++) {
        Page page = error.getPage(pageNumber);
        String title = page.getTitle();
        if (pagesWithError.contains(title)) {
          pagesWithMultipleErrors.add(title);
        } else {
          pagesWithError.add(title);
        }
      }
    }
    List<String> result = new ArrayList<String>(pagesWithMultipleErrors);
    Collections.sort(result);
    return result;
  }

  /**
   * @param pageTitle Page title.
   * @return List of errors for the page.
   */
  List<CheckError> getErrorsForPage(String pageTitle) {
    List<CheckError> result = new ArrayList<CheckError>();
    if (pageTitle != null) {
      for (CheckError error : errors) {
        for (int pageNumber = 0; pageNumber < error.getPageCount(); pageNumber++) {
          Page page = error.getPage(pageNumber);
          String title = page.getTitle();
          if (Page.areSameTitle(pageTitle, title)) {
            result.add(error);
          }
        }
      }
    }
    return result;
  }

  /**
   * Action called when an error type is selected.
   */
  void actionSelectErrorType() {
    Object selection = listAllErrors.getSelectedItem();
    modelPages.clear();
    if (selection instanceof CheckError) {
      CheckError error = (CheckError) selection;

      // Button status
      buttonReloadError.setEnabled(true);
      buttonErrorDetail.setEnabled(true);
      buttonErrorList.setEnabled(true);
      buttonWhiteList.setEnabled(true);
      displayErrorDescription();

      // Pages
      int nbPages = error.getPageCount();
      for (int numPage = 0; numPage < nbPages; numPage++) {
        Page page = error.getPage(numPage);
        CheckErrorPage errorPage = new CheckErrorPage(page, error.getAlgorithm());
        if ((errorPage.isInWhiteList()) && (page.getPageId() != null)) {
          markPageAsFixed(error.getAlgorithm().getErrorNumberString(), page);
        } else {
          modelPages.addElement(errorPage);
        }
      }
      setPageLoaded(false);
      actionSelectPages();
      updateComponentState();
    } else {
      buttonReloadError.setEnabled(false);
      buttonErrorDetail.setEnabled(false);
      buttonErrorList.setEnabled(false);
      buttonWhiteList.setEnabled(false);
      displayErrorDescription();

      if (selection instanceof String) {
        List<String> listErrorPages = getPagesWithSeveralErrors();
        for (String page : listErrorPages) {
          CheckErrorPage errorPage = new CheckErrorPage(DataManager.getPage(getWikipedia(), page, null, null, null), null);
          modelPages.addElement(errorPage);
        }
      }

      setPageLoaded(false);
      actionSelectPages();
      updateComponentState();
    }
  }

  /**
   * Display description of an error.
   */
  public void displayErrorDescription() {
    CheckErrorAlgorithm algorithm = null;

    // Look in the current page
    if ((contentPane != null) &&
        (contentPane.getSelectedComponent() != null) &&
        (contentPane.getSelectedComponent() instanceof CheckWikiContentPanel)) {
      CheckWikiContentPanel panel = (CheckWikiContentPanel) contentPane.getSelectedComponent();
      CheckErrorPage error = panel.getSelectedError();
      if (error != null) {
        algorithm = error.getAlgorithm();
      }
    }

    // Look in the global list of errors
    if (algorithm == null) {
      Object selection = listAllErrors.getSelectedItem();
      if (selection instanceof CheckError) {
        CheckError error = (CheckError) selection;
        algorithm = error.getAlgorithm();
      }
    }

    // Display description
    displayErrorDescription(algorithm);
  }

  /**
   * Display description of an error.
   * 
   * @param algorithm Algorithm.
   */
  private void displayErrorDescription(CheckErrorAlgorithm algorithm) {

    // Check error number
    int errorNumber = -1;
    if (algorithm != null) {
      errorNumber = algorithm.getErrorNumber();
    }
    if (errorNumber == lastErrorDisplayed) {
      return;
    }
    lastErrorDisplayed = errorNumber;

    String description = null;
    if (algorithm != null) {
      description = algorithm.getLongDescription();
      if ((description == null) || (description.trim().length() == 0)) {
        description = algorithm.getShortDescription();
      }
    }
    // Display description
    if ((algorithm != null) && (description != null)) {

      // Error type description
      textDescription.setText(description);

      // Parameters description
      Configuration config = Configuration.getConfiguration();
      boolean secured = config.getBoolean(null, ConfigurationValueBoolean.SECURE_URL);
      EnumWikipedia wiki = getWikipedia();
      String translationPage = wiki.getConfiguration().getString(
          WPCConfigurationString.CW_TRANSLATION_PAGE);
      String url = wiki.getSettings().getURL(translationPage, true, secured);
      StringBuilder parametersDescription = new StringBuilder();
      parametersDescription.append(GT._(
          "The error n°{0} can be configured with the following parameters in the <a href=\"{1}\">translation file</a> :",
          new Object[] { Integer.toString(errorNumber), url }));
      parametersDescription.append("\n<ul>");
      Map<String, String> parameters = algorithm.getParameters();
      for (Map.Entry<String, String> entry : parameters.entrySet()) {
        parametersDescription.append("<li><b>");
        parametersDescription.append(entry.getKey());
        parametersDescription.append("</b>: ");
        parametersDescription.append(entry.getValue());
        parametersDescription.append("</li>\n");
      }
      parametersDescription.append("</ul>");
      textParameters.setText(parametersDescription.toString());
    } else {
      textDescription.clearText();
      textParameters.clearText();
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    super.actionPerformed(e);
    if ((e.getActionCommand() != null) &&
        (e.getActionCommand().startsWith(ACTION_SELECT_ERRORS))) {
      actionSelectErrors(e.getActionCommand().substring(ACTION_SELECT_ERRORS.length()));
    }
  }

  /**
   * Action called when the Select Errors button is pressed. 
   */
  public void actionChooseErrors() {
    popupSelectErrors.show(
        buttonSelectErrors,
        0,
        buttonSelectErrors.getHeight());
  }

  /**
   * Action called when selected errors changes.
   * 
   * @param command Command.
   */
  private void actionSelectErrors(String command) {
    if (command == null) {
      return;
    }

    // Select all
    if (command.equals("*")) {
      selectedAlgorithms.clear();
      for (CheckErrorAlgorithm algorithm : allAlgorithms) {
        if (algorithm.isAvailable() &&
            algorithm.hasList() &&
            CWConfigurationError.isPriorityFullyActive(algorithm.getPriority())) {
          selectedAlgorithms.add(algorithm);
        }
      }

    // Save current selection
    } else if (command.equals("S")) {
      // Retrieve configuration name
      String name = askForValue(GT._("What is the name of the selection?"), null, null);
      if ((name == null) || (name.isEmpty())) {
        return;
      }

      // Creates a string representing the current selection
      StringBuilder selection = new StringBuilder();
      Collections.sort(selectedAlgorithms, new CheckErrorAlgorithmComparator());
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        if (selection.length() > 0) {
          selection.append(",");
        }
        selection.append(algorithm.getErrorNumber());
      }
      String strSelection = selection.toString();

      // Save configuration
      Configuration config = Configuration.getConfiguration();
      config.setSubString(null, Configuration.ARRAY_CHECK_SELECTION, name, strSelection);
      updateSavedSelections();

    // Delete a saved selection
    } else if (command.startsWith("D")) {
      String name = command.substring(1);
      Configuration config = Configuration.getConfiguration();
      config.setSubString(null, Configuration.ARRAY_CHECK_SELECTION, name, null);
      updateSavedSelections();
      
    } else {
      boolean selectionCleared = false;
      String[] units = command.split(",");
      for (int i = 0; i < units.length; i++) {
        String unit = units[i].trim();
  
        // Select priority
        if (unit.startsWith("P")) {
          if (!selectionCleared) {
            selectedAlgorithms.clear();
            selectionCleared = true;
          }
          try {
            int priority = Integer.parseInt(unit.substring(1));
            for (CheckErrorAlgorithm algorithm : allAlgorithms) {
              if (algorithm.isAvailable() &&
                  algorithm.hasList() &&
                  CWConfigurationError.isPriorityActive(algorithm.getPriority()) &&
                  (priority == algorithm.getPriority())) {
                selectedAlgorithms.add(algorithm);
              }
            }
          } catch (NumberFormatException e) {
            //
          }
  
        // Invert error selection
        } else if (unit.startsWith("+")) {
          try {
            int errorNumber = Integer.parseInt(unit.substring(1));
            for (CheckErrorAlgorithm algorithm : allAlgorithms) {
              if (algorithm.isAvailable() &&
                  CWConfigurationError.isPriorityActive(algorithm.getPriority()) &&
                  (errorNumber == algorithm.getErrorNumber())) {
                if (selectedAlgorithms.contains(algorithm)) {
                  selectedAlgorithms.remove(algorithm);
                } else {
                  selectedAlgorithms.add(algorithm);
                }
              }
            }
          } catch (NumberFormatException e) {
            //
          }
  
        // Add an error
        } else {
          if (!selectionCleared) {
            selectedAlgorithms.clear();
            selectionCleared = true;
          }
          try {
            int errorNumber = Integer.parseInt(unit);
            for (CheckErrorAlgorithm algorithm : allAlgorithms) {
              if (algorithm.isAvailable() &&
                  algorithm.hasList() &&
                  CWConfigurationError.isPriorityActive(algorithm.getPriority()) &&
                  (errorNumber == algorithm.getErrorNumber())) {
                if (!selectedAlgorithms.contains(algorithm)) {
                  selectedAlgorithms.add(algorithm);
                }
              }
            }
          } catch (NumberFormatException e) {
            //
          }
        }
      }
    }
    updatePopupSelectErrors();
    buttonLoadErrors.setText(getLoadMessage());
  }

  /**
   * Action called to display error detail. 
   */
  public void actionErrorDetail() {
    Object selected = listAllErrors.getSelectedItem();
    if ((selected instanceof CheckError) &&
        (Utilities.isDesktopSupported())) {
      EnumWikipedia wiki = getWikipedia();
      CheckError error = (CheckError) selected;
      if (error.getAlgorithm().getLink() != null) {
        Utilities.browseURL(wiki, error.getAlgorithm().getLink(), true);
      } else {
        DecimalFormat format = new DecimalFormat("000");
        String description =
            "error_" +
            format.format(error.getErrorNumber()) +
            "_link_" +
            wiki.getSettings().getCodeCheckWiki();
        String translationPage = wiki.getConfiguration().getString(
            WPCConfigurationString.CW_TRANSLATION_PAGE);
        Utilities.displayInformationMessage(getParentComponent(), GT._(
            "There''s no page defined for this error type.\n" +
            "If you want to define a page you need to add :\n" +
            "  {0} = <page name> END\n" +
            "to the translation page ({1}) on \"{2}\"",
            new Object[] { description, translationPage, wiki.toString()
            }));
      }
    }
  }

  /**
   * Action called to display error list on server. 
   */
  public void actionErrorList() {
    Object selected = listAllErrors.getSelectedItem();
    if ((selected instanceof CheckError) &&
        (Utilities.isDesktopSupported())) {
      CheckError error = (CheckError) selected;
      CheckWiki checkWiki = APIFactory.getCheckWiki();
      String url = checkWiki.getUrlDescription(getWikipedia(), error.getAlgorithm());
      Utilities.browseURL(url);
    }
  }

  /**
   * Action called to display error white list. 
   */
  public void actionErrorWhiteList() {
    Object selected = listAllErrors.getSelectedItem();
    if ((selected instanceof CheckError) &&
        (Utilities.isDesktopSupported())) {
      EnumWikipedia wiki = getWikipedia();
      CheckError error = (CheckError) selected;
      if (error.getAlgorithm().getWhiteListPageName() != null) {
        Utilities.browseURL(wiki, error.getAlgorithm().getWhiteListPageName(), true);
      } else {
        DecimalFormat format = new DecimalFormat("000");
        String parameter =
            "error_" +
            format.format(error.getErrorNumber()) +
            "_whitelistpage_" +
            wiki.getSettings().getCodeCheckWiki();
        String translationPage = wiki.getConfiguration().getString(
            WPCConfigurationString.CW_TRANSLATION_PAGE);
        Utilities.displayInformationMessage(getParentComponent(), GT._(
            "There''s no whitelist defined for this error type.\n" +
            "If you want to define a whitelist, you need to add:\n" +
            "  {0} = <page name> END\n" +
            "to the translation page ({1}) on \"{2}\"",
            new Object[] { parameter, translationPage, wiki.toString()
            }));
      }
    }
  }

  /**
   * Action called for selecting pages.
   */
  public void actionSelectPages() {
    Configuration config = Configuration.getConfiguration();
    int max = config.getInt(null, ConfigurationValueInteger.MAXIMUM_PAGES);
    if (max > modelPages.getSize()) {
      max = modelPages.getSize();
    }
    if (max <= 0) {
      listPages.clearSelection();
      return;
    }
    listPages.getSelectionModel().setSelectionInterval(0, max -1);
  }

  /**
   * Action called when requesting to load selected pages.
   */
  public void actionLoadPages() {
    Object[] selection = listPages.getSelectedValues();
    final List<Page> pages = new ArrayList<Page>();
    if (selection != null) {
      for (int i = 0; i < selection.length; i++) {
        CheckErrorPage errorPage = (CheckErrorPage) selection[i];
        pages.add(errorPage.getPage());
      }
    }
    if (pages.size() > 0) {
      RetrieveContentWorker contentWorker = new RetrieveContentWorker(getWikipedia(), this, pages);
      contentWorker.setListener(new DefaultBasicWorkerListener() {

        /* (non-Javadoc)
         * @see org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener#beforeFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker)
         */
        @Override
        public void beforeFinished(BasicWorker worker) {
          super.beforeFinished(worker);
          final List<CheckWikiContentPanel> contentPanels = new ArrayList<CheckWikiContentPanel>();
          for (Page page : pages) {
            while (page != null) {
              Object errorSelected = modelAllErrors.getSelectedItem();
              final CheckWikiContentPanel contentPanel = createContentsComponents(
                  contentPane, page,
                  (errorSelected instanceof CheckError) ?
                      Collections.singletonList((CheckError) errorSelected) :
                      getErrorsForPage(page.getTitle()));
              contentPane.add(contentPanel);
              contentPane.setSelectedComponent(contentPanel);
              contentPanels.add(contentPanel);
              if (page.isRedirect()) {
                List<Page> redirects = page.getRedirects();
                if ((redirects != null) && (redirects.size() > 0)) {
                  page = redirects.get(0);
                } else {
                  page = null;
                }
              } else {
                page = null;
              }
            }
          }
          yesAll = false;
          noAll = false;
          for (CheckWikiContentPanel contentPanel : contentPanels) {
            contentPanel.actionPageSelected();
          }
        }
        //
      });
      contentWorker.start();
    } else {
      updateComponentState();
    }
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    contentPane.removeAll();
    if (errors == null) {
      errors = new ArrayList<CheckError>();
    }
    CheckWikiProjectWorker reloadWorker = new CheckWikiProjectWorker(
        getWikipedia(), this, errors, selectedAlgorithms,
        true, modelMaxErrors.getNumber().intValue());
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }

  /**
   * Action called when Reload Error button is pressed. 
   */
  public void actionReloadError() {
    Object selected = listAllErrors.getSelectedItem();
    if (selected instanceof CheckError) {
      CheckError error = (CheckError) selected;
      List<CheckErrorAlgorithm> algorithms = Collections.singletonList(error.getAlgorithm());
      CheckWikiProjectWorker reloadWorker = new CheckWikiProjectWorker(
          getWikipedia(), this, errors, algorithms,
          true, modelMaxErrors.getNumber().intValue());
      setupReloadWorker(reloadWorker);
      reloadWorker.start();
    }
  }

  /**
   * @param page
   * @param errorNumber
   * @see org.wikipediacleaner.api.check.CheckWikiListener#pageFixed(org.wikipediacleaner.api.data.Page, int)
   */
  @Override
  public void pageFixed(Page page, int errorNumber) {
    if ((errors == null) || (errors.isEmpty())) {
      return;
    }
    Iterator<CheckError> itError = errors.iterator();
    while (itError.hasNext()) {
      CheckError error = itError.next();
      if (error.getErrorNumber() == errorNumber) {
        error.remove(page);
      }
    }
    requestUpdate();
  }

  private boolean updateNeeded = false;

  /**
   * Request for an update of the display.
   */
  private void requestUpdate() {
    updateNeeded = true;
    if (SwingUtilities.isEventDispatchThread()) {
      doUpdate();
    } else {
      SwingUtilities.invokeLater(new Runnable() {
        
        @Override
        public void run() {
          doUpdate();
        }
      });
    }
  }

  /**
   * Update the display. Needs to be run on the event dispatcher thread.
   */
  void doUpdate() {
    if (!updateNeeded) {
      return;
    }
    updateNeeded = false;

    // Remove errors with no pages
    Configuration config = Configuration.getConfiguration();
    boolean showAllErrors = config.getBoolean(
        null,
        ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS);
    if (!showAllErrors) {
      Iterator<CheckError> itError = errors.iterator();
      while (itError.hasNext()) {
        CheckError error = itError.next();
        if (error.getPageCount() == 0) {
          itError.remove();
          modelAllErrors.removeElement(error);
        }
      }
    }

    // Remove list of pages with several errors if needed
    if ((modelAllErrors.getSize() > 0) &&
        !(modelAllErrors.getElementAt(0) instanceof CheckError)) {
      List<String> severalErrors = getPagesWithSeveralErrors();
      if ((severalErrors != null) && severalErrors.isEmpty()) {
        modelAllErrors.removeElementAt(0);
      }
    }

    // Update display
    actionSelectErrorType();
  }
}
