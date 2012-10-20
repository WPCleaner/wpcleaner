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
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.StringReader;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.lobobrowser.html.HtmlRendererContext;
import org.lobobrowser.html.UserAgentContext;
import org.lobobrowser.html.gui.HtmlPanel;
import org.lobobrowser.html.parser.DocumentBuilderImpl;
import org.lobobrowser.html.test.SimpleUserAgentContext;
import org.w3c.dom.Document;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.Contributions;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.JCloseableTabbedPane;
import org.wikipediacleaner.gui.swing.component.MWHtmlRendererContext;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.component.MWPaneBasicFormatter;
import org.wikipediacleaner.gui.swing.component.MWPaneCheckWikiFormatter;
import org.wikipediacleaner.gui.swing.component.MWPaneCheckWikiPopupListener;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;
import org.wikipediacleaner.gui.swing.worker.CheckWikiProjectWorker;
import org.wikipediacleaner.gui.swing.worker.RetrieveContentWorker;
import org.wikipediacleaner.gui.swing.worker.SendWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Check Wiki Project window.
 */
public class CheckWikiProjectWindow extends OnePageWindow {

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
  private HtmlPanel textDescription;
  private HtmlPanel textParameters;
  private UserAgentContext ucontext;
  private HtmlRendererContext rcontextDescription;
  private HtmlRendererContext rcontextParameters;
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

  public final static String ACTION_ANALYZE_PAGES = "ANALYZE_PAGES";
  public final static String ACTION_ERROR_DETAIL  = "ERROR_DETAIL";
  public final static String ACTION_ERROR_LIST    = "ERROR_LIST";
  public final static String ACTION_LOAD_PAGES    = "LOAD_PAGES";
  public final static String ACTION_RELOAD_ERROR  = "RELOAD_ERROR";
  public final static String ACTION_CHOOSE_ERRORS = "CHOOSE_ERRORS";
  public final static String ACTION_SELECT_ERRORS = "SELECT_ERRORS:";
  public final static String ACTION_WHITE_LIST    = "WHITE_LIST";

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
        CheckWikiProjectWindow.class,
        null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
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
    contentPane.addChangeListener(new ChangeListener() {
      
      public void stateChanged(@SuppressWarnings("unused") ChangeEvent e) {
        displayErrorDescription();
      }
    });
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
      if (algorithm.isAvailable() &&
          CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
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
        if (errorNumber > 0) {
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
        if (errorNumber > 0) {
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

          while (menuItemAlgorithms.size() <= errorNumber) {
            menuItemAlgorithms.add(null);
          }
          menuItemAlgorithms.set(errorNumber, menuItem);

          menuItem = new JMenuItem(label);
          if (!CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
            menuItem.setEnabled(false);
            menuItem.setFont(menuItem.getFont().deriveFont(inactiveAttributes));
          } else if (!algorithm.isAvailable()) {
            menuItem.setEnabled(false);
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
    List<String> selections = config.getStringList(null, Configuration.ARRAY_CHECK_SELECTION);
    JMenuItem menuItem = null;
    for (String selection : selections) {
      menuItem = new JMenuItem(selection);
      menuItem.setActionCommand(ACTION_SELECT_ERRORS + selection);
      menuItem.addActionListener(this);
      menuUseSelection.add(menuItem);

      menuItem = new JMenuItem(selection);
      menuItem.setActionCommand(ACTION_SELECT_ERRORS + "D" + selection);
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
        JMenuItem menuItem = menuItemAlgorithms.get(errorNumber);
        if (menuItem != null) {
          menuItem.setSelected(selectedAlgorithms.contains(algorithm));
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
          CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
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
        GT._("Select errors"), true);
    buttonSelectErrors.setActionCommand(ACTION_CHOOSE_ERRORS);
    buttonSelectErrors.addActionListener(this);
    toolbarLoad.add(buttonSelectErrors);
    buttonLoadErrors = Utilities.createJButton(
        "gnome-view-refresh.png", EnumImageSize.NORMAL,
        getLoadMessage(), true);
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
        GT._("Reload error"), false);
    buttonReloadError.setActionCommand(ACTION_RELOAD_ERROR);
    buttonReloadError.addActionListener(this);
    toolbar.add(buttonReloadError);
    buttonErrorDetail = Utilities.createJButton(
        "tango-help-browser.png", EnumImageSize.NORMAL,
        GT._("Detail"), false);
    buttonErrorDetail.setActionCommand(ACTION_ERROR_DETAIL);
    buttonErrorDetail.addActionListener(this);
    buttonErrorDetail.setEnabled(false);
    toolbar.add(buttonErrorDetail);
    buttonErrorList = Utilities.createJButton(
        "gnome-web-browser.png", EnumImageSize.NORMAL,
        GT._("List on toolserver"), false);
    buttonErrorList.setActionCommand(ACTION_ERROR_LIST);
    buttonErrorList.addActionListener(this);
    buttonErrorList.setEnabled(false);
    toolbar.add(buttonErrorList);
    buttonWhiteList = Utilities.createJButton(
        "gnome-accessories-text-editor.png", EnumImageSize.NORMAL,
        GT._("View or edit white list"), false);
    buttonWhiteList.setActionCommand(ACTION_WHITE_LIST);
    buttonWhiteList.addActionListener(this);
    buttonWhiteList.setEnabled(false);
    toolbar.add(buttonWhiteList);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(toolbar, constraints);
    constraints.gridx = 0;
    constraints.gridy++;

    // Error description
    textDescription = new HtmlPanel();
    ucontext = new SimpleUserAgentContext();
    rcontextDescription = new MWHtmlRendererContext(textDescription, ucontext);
    textDescription.setPreferredSize(new Dimension(500, 100));
    textDescription.setMinimumSize(new Dimension(200, 100));

    // Parameters description
    textParameters = new HtmlPanel();
    rcontextParameters = new MWHtmlRendererContext(textParameters, ucontext);
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
   * @return Page liste components
   */
  private Component createPageListComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Pages")));

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
    JButton buttonLoad = Utilities.createJButton(GT._("&Load pages"));
    buttonLoad.setActionCommand(ACTION_LOAD_PAGES);
    buttonLoad.addActionListener(this);
    toolbarButtons.add(buttonLoad);
    JButton buttonAnalysis = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Full analysis (Alt + &F)"), false);
    buttonAnalysis.setActionCommand(ACTION_ANALYZE_PAGES);
    buttonAnalysis.addActionListener(this);
    toolbarButtons.add(buttonAnalysis);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(toolbarButtons, constraints);
    constraints.gridy++;

    // Page List
    modelPages = new DefaultListModel();
    listPages = new JList(modelPages);
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
        actionSelectPage();
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
   * @param page Page.
   * @return Contents components.
   */
  public CheckWikiContentPanel createContentsComponents(JTabbedPane pane, Page page, CheckError error) {
    CheckWikiContentPanel panel = new CheckWikiContentPanel(pane, page, error);
    panel.initialize();
    return panel;
  }
  
  /**
   * Component for working on a page in the CheckWiki project.
   */
  private class CheckWikiContentPanel
    extends JPanel
    implements ActionListener, ItemListener {

    private static final long serialVersionUID = 1L;

    public final static String ACTION_MARK_AS_FIXED = "MARK_AS_FIXED";

    JTabbedPane pane;
    final Page page;
    final CheckError error;

    private JList listErrors;
    private DefaultListModel modelErrors;
    private List<CheckErrorPage> initialErrors;
    private JTextField textComment;
    private JCheckBox chkAutomaticComment;
    private JButton buttonSend;
    private JButton buttonMarkAsFixed;
    private MWPane textPage;

    /**
     * @param page Page.
     */
    CheckWikiContentPanel(JTabbedPane pane, Page page, CheckError error) {
      super(new GridBagLayout());
      this.pane = pane;
      this.page = page;
      this.error = error;
      setName(page.getTitle());
    }

    /**
     * Initialize window.
     */
    void initialize() {

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

      // Comment
      JPanel panelComment = new JPanel(new GridBagLayout());
      GridBagConstraints constraints2 = new GridBagConstraints();
      constraints2.fill = GridBagConstraints.HORIZONTAL;
      constraints2.gridheight = 1;
      constraints2.gridwidth = 1;
      constraints2.gridx = 0;
      constraints2.gridy = 0;
      constraints2.insets = new Insets(0, 0, 0, 0);
      constraints2.ipadx = 0;
      constraints2.ipady = 0;
      constraints2.weightx = 0;
      constraints2.weighty = 0;
      chkAutomaticComment = createChkAutomaticComment(true, this);
      panelComment.add(chkAutomaticComment, constraints2);
      constraints2.gridx++;
      textComment = new JTextField(getComment(null));
      textComment.setEditable(false);
      constraints2.weightx = 1;
      panelComment.add(textComment, constraints2);
      constraints2.gridx++;
      constraints.fill = GridBagConstraints.HORIZONTAL;
      constraints.gridwidth = 2;
      constraints.weightx = 1;
      constraints.weighty = 0;
      add(panelComment, constraints);
      constraints.gridy++;

      // Buttons
      JToolBar toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
      toolbarButtons.setFloatable(false);
      JButton buttonFirst = createButtonFirstOccurence(this, true);
      toolbarButtons.add(buttonFirst);
      JButton buttonPrev = createButtonPreviousOccurence(this, true);
      toolbarButtons.add(buttonPrev);
      JButton buttonNext = createButtonNextOccurence(this, true);
      toolbarButtons.add(buttonNext);
      JButton buttonLast = createButtonLastOccurence(this, true);
      toolbarButtons.add(buttonLast);
      toolbarButtons.addSeparator();
      JButton buttonToc = createButtonToc(this, true);
      toolbarButtons.add(buttonToc);
      JButton buttonValidate = createButtonValidate(this, true);
      toolbarButtons.add(buttonValidate);
      buttonSend = createButtonSend(this, true);
      buttonSend.setEnabled(false);
      toolbarButtons.add(buttonSend);
      buttonMarkAsFixed = Utilities.createJButton(GT._("Mark as Fixed")); // Mark as fixed
      buttonMarkAsFixed.setEnabled(true);
      buttonMarkAsFixed.setActionCommand(ACTION_MARK_AS_FIXED);
      buttonMarkAsFixed.addActionListener(this);
      toolbarButtons.add(buttonMarkAsFixed);
      toolbarButtons.addSeparator();
      if (Utilities.isDesktopSupported()) { // External Viewer
        JButton buttonView = createButtonView(this, true);
        toolbarButtons.add(buttonView);
      }
      if (Utilities.isDesktopSupported()) { // History
        JButton buttonHistory = createButtonViewHistory(this, true);
        toolbarButtons.add(buttonHistory);
        toolbarButtons.addSeparator();
      }
      JButton buttonFullAnalysis = createButtonFullAnalysis(this, true);
      toolbarButtons.add(buttonFullAnalysis);
      constraints.fill = GridBagConstraints.HORIZONTAL;
      constraints.gridwidth = 2;
      constraints.weightx = 1;
      constraints.weighty = 0;
      add(toolbarButtons, constraints);
      constraints.gridy++;

      // Errors list
      modelErrors = new DefaultListModel();
      listErrors = new JList(modelErrors);
      CheckErrorPageListCellRenderer cellRenderer = new CheckErrorPageListCellRenderer(false);
      cellRenderer.showCountOccurence(true);
      listErrors.setCellRenderer(cellRenderer);
      listErrors.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      listErrors.addListSelectionListener(new ListSelectionListener() {

        public void valueChanged(ListSelectionEvent e) {
          if (e.getValueIsAdjusting()) {
            return;
          }
          actionSelectError();
        }
        
      });
      JScrollPane scrollErrors = new JScrollPane(listErrors);
      scrollErrors.setMinimumSize(new Dimension(200, 200));
      scrollErrors.setPreferredSize(new Dimension(200, 300));
      scrollErrors.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      constraints.fill = GridBagConstraints.BOTH;
      constraints.gridwidth = 1;
      constraints.gridx = 0;
      constraints.weightx = 0;
      constraints.weighty = 1;
      add(scrollErrors, constraints);
      constraints.gridx++;

      // Page contents
      textPage = new MWPane(getWikipedia(), page, CheckWikiProjectWindow.this);
      listErrors.addMouseListener(
          new CheckErrorPageListPopupListener(getWikipedia(), textPage, buttonValidate));
      textPage.setEditable(true);
      textPage.addPropertyChangeListener(
          MWPane.PROPERTY_MODIFIED,
          new PropertyChangeListener() {
  
            /* (non-Javadoc)
             * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
             */
            public void propertyChange(@SuppressWarnings("unused") PropertyChangeEvent evt) {
              updateComponentState();
            }
            
          });
      textPage.setPopupListener(new MWPaneCheckWikiPopupListener(
          getWikipedia(), CheckWikiProjectWindow.this));
      JComponent scrollContents = MWPane.createComplexPane(textPage);
      scrollContents.setMinimumSize(new Dimension(100, 100));
      constraints.fill = GridBagConstraints.BOTH;
      constraints.weightx = 1;
      constraints.weighty = 1;
      //panel.add(scrollPage, constraints);
      add(scrollContents, constraints);
      constraints.gridy++;
    }

    /**
     * Update component state.
     */
    public void updateComponentState() {
      if (buttonSend != null) {
        buttonSend.setEnabled((textPage != null) && (textPage.isModified()));
      }
      if (buttonMarkAsFixed != null) {
        buttonMarkAsFixed.setEnabled(error != null);
      }
    }

    /**
     * Action called when a page is selected (after page is loaded).
     */
    @SuppressWarnings("fallthrough")
    void actionPageSelected() {
      if (page == null) {
        pane.remove(this);
        return;
      }
      if (Boolean.FALSE.equals(page.isExisting())) {
        displayWarning(GT._("The page {0} doesn't exist on Wikipedia", page.getTitle()));
        if (error != null) {
          error.remove(page);
        }
        pane.remove(this);
        if (error != null) {
          markPageAsFixed(error, error.getAlgorithm().getErrorNumberString(), page);
        }
        actionSelectErrorType();
        return;
      }
      textPage.setText(page.getContents());
      textPage.setModified(false);
      PageAnalysis pageAnalysis = new PageAnalysis(page, textPage.getText());
      List<CheckErrorPage> errorsFound = CheckError.analyzeErrors(
          allAlgorithms, pageAnalysis);
      modelErrors.clear();
      initialErrors = new ArrayList<CheckErrorPage>();
      boolean errorFound = false;
      int errorCount = 0;
      if (errorsFound != null) {
        for (CheckErrorPage tmpError : errorsFound) {
          modelErrors.addElement(tmpError);
          initialErrors.add(tmpError);
          errorCount++;
          if ((error != null) &&
              (error.getAlgorithm() != null) &&
              (error.getAlgorithm().getErrorNumber()== tmpError.getAlgorithm().getErrorNumber())) {
            errorFound = true;
          }
        }
      }
      if ((error != null) && (errorFound == false) && (error.getAlgorithm().isFullDetection())) {
        Configuration config = Configuration.getConfiguration();
        int answer = JOptionPane.YES_OPTION;
        if (yesAll) {
          answer = Utilities.YES_ALL_OPTION;
        } else if (noAll) {
          answer = Utilities.NO_ALL_OPTION;
        } else {
          if (!config.getBoolean(
              null,
              ConfigurationValueBoolean.CHECK_MARK_AS_FIXED)) {
            answer = displayYesNoAllWarning(GT._(
                "The error n°{0} hasn''t been found in the page {1}.\n" +
                "Do you want to mark it as fixed ?",
                new Object[] { error.getAlgorithm().getErrorNumberString(), page.getTitle() }));
          }
        }
        switch (answer) {
        case Utilities.YES_ALL_OPTION:
          yesAll = true;
          // Go through
        case JOptionPane.YES_OPTION:
          error.remove(page);
          if (errorCount == 0) {
            pane.remove(this);
          }
          markPageAsFixed(error, error.getAlgorithm().getErrorNumberString(), page);
          actionSelectErrorType();
          return;
        case Utilities.NO_ALL_OPTION:
          noAll = true;
          break;
        }
      }
      int index = modelErrors.indexOf(listAllErrors.getSelectedItem());
      if (index >= 0) {
        listErrors.setSelectedIndex(index);
      } else if (modelErrors.getSize() > 0) {
        listErrors.setSelectedIndex(0);
      }

      // Automatic fix of some errors
      if ((initialErrors != null) && (textPage != null)) {
        String initialContents = textPage.getText();
        String contents = initialContents;
        for (CheckErrorPage initialError : initialErrors) {
          contents = initialError.getAlgorithm().automaticFix(page, contents);
        }
        if (!contents.equals(initialContents)) {
          textPage.changeText(contents);
          actionValidate();
        }
      }
    }

    /**
     * @return Current selected error.
     */
    public CheckErrorPage getSelectedError() {
      Object selection = listErrors.getSelectedValue();
      if (selection instanceof CheckErrorPage) {
        return (CheckErrorPage) selection;
      }
      return null;
    }

    /**
     * Action called when an error is selected. 
     */
    void actionSelectError() {
      CheckErrorPage errorSelected = getSelectedError();
      if (errorSelected == null) {
        textPage.setFormatter(new MWPaneBasicFormatter());
      } else {
        CheckErrorAlgorithm algorithm = errorSelected.getAlgorithm();
        MWPaneFormatter formatter = textPage.getFormatter();
        if (formatter instanceof MWPaneCheckWikiFormatter) {
          MWPaneCheckWikiFormatter cwFormatter =
            (MWPaneCheckWikiFormatter) formatter;
          if (!cwFormatter.isSameAlgorithm(algorithm)) {
            formatter = new MWPaneCheckWikiFormatter(algorithm);
            textPage.setFormatter(formatter);
          } else {
            textPage.resetAttributes();
          }
        } else {
          formatter = new MWPaneCheckWikiFormatter(algorithm);
          textPage.setFormatter(formatter);
        }
      }
      listErrors.repaint();
      updateComponentState();
      displayErrorDescription();
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
      if (e == null) {
        return;
      }

      if (ACTION_FIRST_OCCURRENCE.equals(e.getActionCommand())) {
        actionFirstOccurrence();
      } else if (ACTION_FULL_ANALYSIS_PAGE.equals(e.getActionCommand())) {
        Controller.runFullAnalysis(page.getTitle(), null, getWikipedia());
      } else if (ACTION_LAST_OCCURRENCE.equals(e.getActionCommand())) {
        actionLastOccurrence();
      } else if (ACTION_MARK_AS_FIXED.equals(e.getActionCommand())) {
        actionMarkAsFixed();
      } else if (ACTION_NEXT_OCCURRENCE.equals(e.getActionCommand())) {
        actionNextOccurrence();
      } else if (ACTION_PREVIOUS_OCCURRENCE.equals(e.getActionCommand())) {
        actionPreviousOccurrence();
      } else if (ACTION_SEND.equals(e.getActionCommand())) {
        actionSend();
      } else if (ACTION_TOC.equals(e.getActionCommand())) {
        actionToc();
      } else if (ACTION_VALIDATE.equals(e.getActionCommand())) {
        actionValidate();
      } else if (ACTION_VIEW.equals(e.getActionCommand())) {
        actionView();
      } else if (ACTION_VIEW_HISTORY.equals(e.getActionCommand())) {
        actionViewHistory();
      }
    }

    /**
     * Mark a page as fixed. 
     */
    private void actionMarkAsFixed() {

      // Ask for confirmation
      if (displayYesNoWarning(GT._(
          "Do you want to mark {0} as fixed for error n°{1}",
          new Object[] { page.getTitle(), Integer.toString(error.getErrorNumber())})) != JOptionPane.YES_OPTION) {
        return;
      }

      // Check if error is still present
      PageAnalysis pageAnalysis = new PageAnalysis(page, textPage.getText());
      CheckErrorPage errorPage = CheckError.analyzeError(
          error.getAlgorithm(), pageAnalysis);
      if ((errorPage.getResults() != null) &&
          (!errorPage.getResults().isEmpty())) {
        if (displayYesNoWarning(GT._(
            "The error n°{0} is still found {1} times in the page.\n" +
            "Are you really sure that you want to mark it as fixed ?",
            new Object[] { Integer.toString(error.getErrorNumber()), errorPage.getResults().size() } )) != JOptionPane.YES_OPTION) {
          return;
        }
      } else if (errorPage.getErrorFound()) {
        if (displayYesNoWarning(GT._(
            "The error n°{0} is still found in the page.\n" +
            "Are you really sure that you want to mark it as fixed ?",
            Integer.toString(error.getErrorNumber()))) != JOptionPane.YES_OPTION) {
          return;
        }
      } else {
        // Check if error was initially present
        for (int i = 0; i < modelErrors.size(); i++) {
          if (modelErrors.elementAt(i) instanceof CheckErrorPage) {
            CheckErrorPage tmp = (CheckErrorPage) modelErrors.elementAt(i);
            if (tmp.getAlgorithm() == error.getAlgorithm()) {
              displayWarning(GT._(
                  "You have already fixed this error by modifying the page.\n" +
                  "You should send your modifications, the page will be marked as fixed."));
              return;
            }
          }
        }
      }

      // Mark as fixed
      error.remove(page);
      pane.remove(CheckWikiContentPanel.this);
      if (error.getPageCount() == 0) {
        Configuration configuration = Configuration.getConfiguration();
        if (!configuration.getBoolean(
            null,
            ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS)) {
          listAllErrors.removeItem(error);
        }
      }
      actionSelectErrorType();
      markPageAsFixed(error, error.getAlgorithm().getErrorNumberString(), page);
    }

    /**
     * Select first occurrence.
     */
    private void actionFirstOccurrence() {
      textPage.getSelectionManager().selectFirstOccurrence();
      textPage.requestFocusInWindow();
    }

    /**
     * Select previous occurrence. 
     */
    private void actionPreviousOccurrence() {
      textPage.getSelectionManager().selectPreviousOccurrence();
      textPage.requestFocusInWindow();
    }

    /**
     * Select next occurrence.
     */
    private void actionNextOccurrence() {
      textPage.getSelectionManager().selectNextOccurrence();
      textPage.requestFocusInWindow();
    }

    /**
     * Select last occurrence.
     */
    private void actionLastOccurrence() {
      textPage.getSelectionManager().selectLastOccurrence();
      textPage.requestFocusInWindow();
    }

    /**
     * Compute comment.
     * 
     * @param errorsFixed Errors fixed
     * @return Comment.
     */
    private String getComment(List<CheckErrorAlgorithm> errorsFixed) {
      StringBuilder comment = new StringBuilder();
      if (errorsFixed != null) {
        for (int pos = 0; pos < errorsFixed.size(); pos++) {
          if (pos > 0) {
            comment.append(" - ");
          }
          String link = errorsFixed.get(pos).getLink();
          Configuration config = Configuration.getConfiguration();
          if ((link != null) &&
              (config != null) &&
              (config.getBoolean(
                  null,
                  ConfigurationValueBoolean.CHECK_LINK_ERRORS))) {
            comment.append("[[");
            comment.append(link);
            comment.append("|");
            comment.append(errorsFixed.get(pos).getShortDescriptionReplaced());
            comment.append("]]");
          } else {
            comment.append(errorsFixed.get(pos).getShortDescriptionReplaced());
          }
        }
      }
      if (comment.length() > 0) {
        comment.append(" (");
        comment.append(getAutomaticComment(null));
        comment.append(")");
      }
      return comment.toString();
    }

    /**
     * @return Errors fixed.
     */
    private List<CheckErrorAlgorithm> computeErrorsFixed() {
      final List<CheckErrorAlgorithm> errorsFixed = new ArrayList<CheckErrorAlgorithm>();
      PageAnalysis pageAnalysis = null;
      if (initialErrors != null) {
        for (CheckErrorPage initialError : initialErrors) {
          if (pageAnalysis == null) {
            pageAnalysis = new PageAnalysis(initialError.getPage(), textPage.getText());
          }
          CheckErrorPage errorPage = CheckError.analyzeError(
              initialError.getAlgorithm(), pageAnalysis);
          if ((errorPage.getErrorFound() == false) ||
              (errorPage.getActiveResultsCount() < initialError.getActiveResultsCount())) {
            errorsFixed.add(initialError.getAlgorithm());
          }
        }
      }
      return errorsFixed;
    }

    /**
     * Send page.
     */
    private void actionSend() {
      // Check page text to see what errors are still present
      final List<CheckErrorAlgorithm> errorsFixed = computeErrorsFixed();
      updateComment(errorsFixed);

      // Check that a comment is available
      if (textComment.getText().trim().length() == 0) {
        Utilities.displayWarning(getParent(), GT._(
            "A comment is required for sending the page."));
        return;
      }

      // Count contributions
      Contributions contributions = new Contributions(getWikipedia());
      contributions.increasePages(1);
      for (CheckErrorAlgorithm algorithm : errorsFixed) {
        contributions.increaseCheckWikiError(algorithm.getErrorNumber(), 1);
      }

      // Send page
      final Configuration configuration = Configuration.getConfiguration();
      SendWorker sendWorker = new SendWorker(
          getWikipedia(), CheckWikiProjectWindow.this,
          page, textPage.getText(), textComment.getText(),
          configuration.getBoolean(
              null,
              ConfigurationValueBoolean.FORCE_WATCH),
          false, false,
          contributions, errorsFixed);
      sendWorker.setListener(new DefaultBasicWorkerListener() {
        @Override
        public void afterFinished(
            @SuppressWarnings("unused") BasicWorker worker,
            boolean ok) {
          if (ok) {
            // Close pane
            pane.remove(CheckWikiContentPanel.this);

            // Remove errors fixed
            List<CheckError> errorsToBeRemoved = new ArrayList<CheckError>();
            for (CheckErrorAlgorithm algoFixed : errorsFixed) {
              for (int posError = 0; posError < modelAllErrors.getSize(); posError++) {
                Object element = modelAllErrors.getElementAt(posError);
                if (element instanceof CheckError) {
                  final CheckError tmpError = (CheckError) element;
                  if (tmpError.getAlgorithm().getErrorNumberString().equals(algoFixed.getErrorNumberString())) {
                    tmpError.remove(page);
                    if (tmpError.getPageCount() == 0) {
                      errorsToBeRemoved.add(tmpError);
                    }
                  }
                }
              }
            }
            if (!configuration.getBoolean(
                null,
                ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS)) {
              for (CheckError tmpError : errorsToBeRemoved) {
                listAllErrors.removeItem(tmpError);
              }
            }
            actionSelectErrorType();
          }
        }
      });
      sendWorker.start();
    }

    /**
     * Display table of contents.
     */
    private void actionToc() {
      textPage.toggleToc();
    }

    /**
     * Validate current text and recompute errors.
     */
    private void actionValidate() {
      // Check for new errors
      PageAnalysis pageAnalysis = new PageAnalysis(page, textPage.getText());
      List<CheckErrorPage> errorsFound = CheckError.analyzeErrors(
          allAlgorithms, pageAnalysis);
      if (errorsFound != null) {
        for (CheckErrorPage tmpError : errorsFound) {
          boolean errorFound = false;
          for (int index = 0; index < modelErrors.getSize(); index++) {
            CheckErrorPage errorModel = (CheckErrorPage) modelErrors.get(index);
            if ((errorModel != null) &&
                (errorModel.getAlgorithm() != null) &&
                (errorModel.getAlgorithm().equals(tmpError.getAlgorithm()))) {
              errorFound = true;
              modelErrors.set(index, tmpError);
            }
          }
          if (!errorFound) {
            modelErrors.addElement(tmpError);
          }
        }
      }
      for (int index = 0; index < modelErrors.getSize(); index++) {
        CheckErrorPage errorModel = (CheckErrorPage) modelErrors.get(index);
        if ((errorsFound == null) || (!errorsFound.contains(errorModel))) {
          CheckErrorPage newError = new CheckErrorPage(page, errorModel.getAlgorithm());
          modelErrors.set(index, newError);
        }
      }

      actionSelectError();
      updateComment(null);
      Object selected = listErrors.getSelectedValue();
      if (selected instanceof CheckErrorPage) {
        CheckErrorPage errorPage = (CheckErrorPage) selected;
        if (!errorPage.getErrorFound()) {
          int index = listErrors.getSelectedIndex();
          if (index < modelErrors.getSize() - 1) {
            listErrors.setSelectedIndex(index + 1);
          }
        }
      }
    }

    /**
     * View page in external viewer.
     */
    private void actionView() {
      Utilities.browseURL(getWikipedia(), page.getTitle(), false);
    }

    /**
     * View page history in external viewer.
     */
    private void actionViewHistory() {
      Utilities.browseURL(getWikipedia(), page.getTitle(), "history");
    }

    /**
     * Update automatic comment.
     * 
     * @param errorsFixed Errors.
     */
    private void updateComment(List<CheckErrorAlgorithm> errorsFixed) {
      if ((chkAutomaticComment != null) &&
          (textComment != null)) {
        textComment.setEditable(!chkAutomaticComment.isSelected());
        if (chkAutomaticComment.isSelected()) {
          textComment.setText(getComment((errorsFixed != null) ? errorsFixed : computeErrorsFixed()));
        }
      }
    }

    /* (non-Javadoc)
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
    public void itemStateChanged(ItemEvent e) {
      if ((e == null) || (e.getSource() == null)) {
        return;
      }
      Object source = e.getSource();
      if ((source == chkAutomaticComment)) {
        updateComment(null);
      }
    }
  }

  /**
   * @param pageAnalysis Page analysis.
   * @return Default comment.
   */
  @Override
  protected String getAutomaticComment(PageAnalysis pageAnalysis) {
    return getWikipedia().getCWConfiguration().getComment();
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.PageWindow#afterFinishedReloadWorker()
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
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
        if (modelAllErrors.getSize() > 1) {
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
          // TODO: Need page id (either retrieve it when needed or wait to have it directly from toolserver)
          markPageAsFixed(error, error.getAlgorithm().getErrorNumberString(), page);
        } else {
          modelPages.addElement(errorPage);
        }
      }
      setPageLoaded(false);
      actionSelectPage();
    } else {
      buttonReloadError.setEnabled(false);
      buttonErrorDetail.setEnabled(false);
      buttonErrorList.setEnabled(false);
      buttonWhiteList.setEnabled(false);
      displayErrorDescription();

      if (selection instanceof String) {
        List<String> listTmp = new ArrayList<String>();
        for (CheckError error : errors) {
          int nbPages = error.getPageCount();
          for (int numPage = 0; numPage < nbPages; numPage++) {
            Page page = error.getPage(numPage);
            listTmp.add(page.getTitle());
          }
        }
        Collections.sort(listTmp);
        List<String> listErrorPages = new ArrayList<String>();
        for (int listPosition = 1; listPosition < listTmp.size(); listPosition++) {
          if (listTmp.get(listPosition - 1).equals(listTmp.get(listPosition))) {
            if (!listErrorPages.contains(listTmp.get(listPosition))) {
              listErrorPages.add(listTmp.get(listPosition));
            }
          }
        }
        for (String page : listErrorPages) {
          CheckErrorPage errorPage = new CheckErrorPage(DataManager.getPage(getWikipedia(), page, null, null), null);
          modelPages.addElement(errorPage);
        }
      }

      setPageLoaded(false);
      actionSelectPage();
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

    // Display description
    if (algorithm != null) {
      // Error type description
      try {
        DocumentBuilderImpl dbi = new DocumentBuilderImpl(ucontext, rcontextDescription);
        InputSource is = new InputSource(new StringReader(algorithm.getLongDescription()));
        is.setSystemId(
            "http://toolserver.org/~sk/cgi-bin/checkwiki/checkwiki.cgi?" +
            "project=frwiki&view=only&id=" + algorithm.getErrorNumber());
        Document document = dbi.parse(is);
        textDescription.setDocument(document, rcontextDescription);
      } catch (SAXException e) {
        textDescription.clearDocument();
      } catch (IOException e) {
        textDescription.clearDocument();
      }

      // Parameters description
      try {
        Configuration config = Configuration.getConfiguration();
        boolean secured = config.getBoolean(null, ConfigurationValueBoolean.SECURE_URL);
        String url =
          getWikipedia().getSettings().getURL(getWikipedia().getCWConfiguration().getTranslationPage(), true, secured);
        StringBuilder parametersDescription = new StringBuilder();
        parametersDescription.append(GT._(
            "The error n°{0} can be configured with the following parameters in the <a href=\"{1}\">translation file</a> :",
            new Object[] { Integer.toString(algorithm.getErrorNumber()), url }));
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
        DocumentBuilderImpl dbi = new DocumentBuilderImpl(ucontext, rcontextParameters);
        InputSource is = new InputSource(new StringReader(parametersDescription.toString()));
        is.setSystemId(url);
        Document document = dbi.parse(is);
        textParameters.setDocument(document, rcontextParameters);
      } catch (SAXException e) {
        textParameters.clearDocument();
      } catch (IOException e) {
        textParameters.clearDocument();
      }
    } else {
      textDescription.clearDocument();
      textParameters.clearDocument();
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
    if (ACTION_ERROR_DETAIL.equals(e.getActionCommand())) {
      actionErrorDetail();
    } else if (ACTION_ERROR_LIST.equals(e.getActionCommand())) {
      actionErrorList();
    } else if (ACTION_LOAD_PAGES.equals(e.getActionCommand())) {
      actionSelectPage();
    } else if (ACTION_ANALYZE_PAGES.equals(e.getActionCommand())) {
      actionAnalyzePage();
    } else if (ACTION_WHITE_LIST.equals(e.getActionCommand())) {
      actionErrorWhiteList();
    } else if (ACTION_RELOAD_ERROR.equals(e.getActionCommand())) {
      actionReloadError();
    } else if (ACTION_CHOOSE_ERRORS.equals(e.getActionCommand())) {
      actionChooseErrors();
    } else if ((e.getActionCommand() != null) &&
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
            CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
          selectedAlgorithms.add(algorithm);
        }
      }

    // Save current selection
    } else if (command.equals("S")) {
      // Creates a string representing the current selection
      StringBuilder selection = new StringBuilder();
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        if (selection.length() > 0) {
          selection.append(",");
        }
        selection.append(algorithm.getErrorNumber());
      }
      String strSelection = selection.toString();

      // Save configuration
      Configuration config = Configuration.getConfiguration();
      List<String> selections = config.getStringList(null, Configuration.ARRAY_CHECK_SELECTION);
      if (!selections.contains(strSelection)) {
        selections.add(strSelection);
        Collections.sort(selections);
        config.setStringList(null, Configuration.ARRAY_CHECK_SELECTION, selections);
      }
      updateSavedSelections();

    // Delete a saved selection
    } else if (command.startsWith("D")) {
      String selection = command.substring(1);
      Configuration config = Configuration.getConfiguration();
      List<String> selections = config.getStringList(null, Configuration.ARRAY_CHECK_SELECTION);
      if (selections.contains(selection)) {
        selections.remove(selection);
        Collections.sort(selections);
        config.setStringList(null, Configuration.ARRAY_CHECK_SELECTION, selections);
      }
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
  private void actionErrorDetail() {
    Object selected = listAllErrors.getSelectedItem();
    if ((selected instanceof CheckError) &&
        (Utilities.isDesktopSupported())) {
      CheckError error = (CheckError) selected;
      if (error.getAlgorithm().getLink() != null) {
        Utilities.browseURL(getWikipedia(), error.getAlgorithm().getLink(), true);
      } else {
        DecimalFormat format = new DecimalFormat("000");
        Utilities.displayInformationMessage(getParentComponent(), GT._(
            "There''s no page defined for this error type.\n" +
            "If you want to define a page you need to add :\n" +
            "  {0} = <page name> END\n" +
            "to the translation page ({1}) on \"{2}\"",
            new Object[] {
                "error_" + format.format(error.getErrorNumber()) + "_link_" + getWikipedia().getSettings().getCodeCheckWiki(),
                getWikipedia().getCWConfiguration().getTranslationPage(),
                getWikipedia().toString()
            }));
      }
    }
  }

  /**
   * Action called to display error list on toolserver. 
   */
  private void actionErrorList() {
    Object selected = listAllErrors.getSelectedItem();
    if ((selected instanceof CheckError) &&
        (Utilities.isDesktopSupported())) {
      CheckError error = (CheckError) selected;
      String url =
        "http://toolserver.org/~sk/cgi-bin/checkwiki/checkwiki.cgi" +
        "?id=" + error.getErrorNumber() +
        "&project=" + getWikipedia().getSettings().getCodeCheckWiki() +
        "&view=only" +
        "&limit=" + modelMaxErrors.getNumber();
      Utilities.browseURL(url);
    }
  }

  /**
   * Action called to display error white list. 
   */
  private void actionErrorWhiteList() {
    Object selected = listAllErrors.getSelectedItem();
    if ((selected instanceof CheckError) &&
        (Utilities.isDesktopSupported())) {
      CheckError error = (CheckError) selected;
      if (error.getAlgorithm().getWhiteListPageName() != null) {
        Utilities.browseURL(getWikipedia(), error.getAlgorithm().getWhiteListPageName(), true);
      } else {
        DecimalFormat format = new DecimalFormat("000");
        Utilities.displayInformationMessage(getParentComponent(), GT._(
            "There''s no white list defined for this error type.\n" +
            "If you want to define a white list you need to add :\n" +
            "  {0} = <page name> END\n" +
            "to the translation page ({1}) on \"{2}\"",
            new Object[] {
                "error_" + format.format(error.getErrorNumber()) + "_whitelistpage_" + getWikipedia().getSettings().getCodeCheckWiki(),
                getWikipedia().getCWConfiguration().getTranslationPage(),
                getWikipedia().toString()
            }));
      }
    }
  }

  /**
   * Action called when a page is selected.
   */
  void actionSelectPage() {
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
                  (errorSelected instanceof CheckError) ? (CheckError) errorSelected : null);
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
   * Action called when a page is analyzed.
   */
  void actionAnalyzePage() {
    Object[] selection = listPages.getSelectedValues();
    if (selection != null) {
      for (int i = 0; i < selection.length; i++) {
        CheckErrorPage errorPage = (CheckErrorPage) selection[i];
        Controller.runFullAnalysis(errorPage.getPage().getTitle(), null, getWikipedia());
      }
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
  protected void actionReloadError() {
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
}
