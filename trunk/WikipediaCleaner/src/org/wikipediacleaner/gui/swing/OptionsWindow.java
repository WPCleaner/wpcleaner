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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.NamedComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * Options Window of WikipediaCleaner. 
 */
public class OptionsWindow
  extends BasicWindow
  implements ActionListener, ListSelectionListener {

  private final static String ACTION_APPLY       = "APPLY";
  private final static String ACTION_CANCEL      = "CANCEL";
  private final static String ACTION_DEFAULT     = "RESTORE_DEFAULT";
  private final static String ACTION_SORT_ADD    = "SORT ADD";
  private final static String ACTION_SORT_DELETE = "SORT DELETE";
  private final static String ACTION_SORT_DOWN   = "SORT DOWN";
  private final static String ACTION_SORT_UP     = "SORT UP";
  private final static String ACTION_VALIDATE    = "VALIDATE";

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  private JCheckBox chkAdvancedFeatures;
  private JCheckBox chkCloseDisambig;
  private JCheckBox chkRestoreWindowPosition;
  private JCheckBox chkSaveWindowPosition;
  private JCheckBox chkShortNotation;
  private JCheckBox chkWikiInComments;
  private JCheckBox chkShow0ErrorsCheckWiki;
  private JCheckBox chkLinkErrorsCheckWiki;
  private SpinnerNumberModel modelMenuSize;
  private JSpinner spinMenuSize;
  private SpinnerNumberModel modelMaxPages;
  private JSpinner spinMaxPages;
  private SpinnerNumberModel modelMaxErrorsCheckWiki;
  private JSpinner spinMaxErrorsCheckWiki;
  private SpinnerNumberModel modelInterrogationThreads;
  private JSpinner spinInterrogationThreads;
  private JTextField txtSignature;

  private JCheckBox chkAnalysisCreateDabWarning;
  private JCheckBox chkAnalysisCreateDabWarningAll;
  private JCheckBox chkAnalysisUpdateDabWarning;
  private JCheckBox chkAnalysisUpdateDabWarningAll;
  private JCheckBox chkAnalysisCountDisambig;
  private JCheckBox chkAnalysisCountMissing;
  private JCheckBox chkAnalysisCountOther;
  private JCheckBox chkAnalysisCountRedirect;
  private JCheckBox chkAnalysisDisambig;
  private JCheckBox chkAnalysisHideSending;
  private JCheckBox chkAnalysisMissing;
  private JCheckBox chkAnalysisOther;
  private JCheckBox chkAnalysisRedirect;
  private JCheckBox chkSaveLastReplacement;
  private JCheckBox chkRememberLastPage;
  private JCheckBox chkCloseFull;
  private JCheckBox chkForceWatch;
  private JSpinner spinAnalysisNbPages;
  private SpinnerNumberModel modelAnalysisNbPages;
  private JSpinner spinAnalysisUndoLevels;
  private SpinnerNumberModel modelAnalysisUndoLevels;

  private JList listSort;
  private DefaultListModel modelSort;
  private JList listSortItem;
  private DefaultListModel modelSortItem;
  private JButton buttonSortAdd;
  private JButton buttonSortDelete;
  private JButton buttonSortUp;
  private JButton buttonSortDown;

  private JCheckBox chkDebugTime;
  private JCheckBox chkDebugURL;
  private JCheckBox chkDebugXML;

  private JButton buttonApply;
  private JButton buttonCancel;
  private JButton buttonDefault;
  private JButton buttonValidate;

  /**
   * Create and display an OptionsWindow.
   */
  public static void createOptionsWindow() {
    createWindow(
        "OptionsWindow",
        null,
        WindowConstants.DISPOSE_ON_CLOSE,
        OptionsWindow.class,
        null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Options");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(createOptionsComponents());
    panel.add(createCommandComponents());
    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createOptionsComponents() {
    JPanel panel = new JPanel(new BorderLayout());

    // Create tabbed pane
    JTabbedPane pane = new JTabbedPane();
    pane.addTab(GT._("General"), createGeneralComponents());
    pane.addTab(GT._("Full analysis"), createAnalysisComponents());
    pane.addTab(GT._("Sorting"), createSortingComponents());
    pane.addTab(GT._("Debug"), createDebugComponents());
    panel.add(pane);

    return panel;
  }

  /**
   * @return General options components.
   */
  private Component createGeneralComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("General options")));
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

    constraints.gridwidth = 3;

    // Close disambiguation window after sending
    chkCloseDisambig = Utilities.createJCheckBox(
        GT._("Close disambiguation window after sending"),
        configuration.getBoolean(
            Configuration.BOOLEAN_CLOSE_DISAMBIG,
            Configuration.DEFAULT_CLOSE_DISAMBIG));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkCloseDisambig, constraints);
    constraints.gridy++;

    // Restore window position
    chkRestoreWindowPosition = Utilities.createJCheckBox(
        GT._("Restore window position"),
        configuration.getBoolean(
            Configuration.BOOLEAN_RESTORE_WINDOW,
            Configuration.DEFAULT_RESTORE_WINDOW));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkRestoreWindowPosition, constraints);
    constraints.gridy++;

    // Save window position
    chkSaveWindowPosition = Utilities.createJCheckBox(
        GT._("Save window position"),
        configuration.getBoolean(
            Configuration.BOOLEAN_SAVE_WINDOW,
            Configuration.DEFAULT_SAVE_WINDOW));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkSaveWindowPosition, constraints);
    constraints.gridy++;

    // Use short notation
    chkShortNotation = Utilities.createJCheckBox(
        GT._("Use short notation [[Xxxxx (yyy)|]]"),
        configuration.getBoolean(
            Configuration.BOOLEAN_SHORT_NOTATION,
            Configuration.DEFAULT_SHORT_NOTATION));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkShortNotation, constraints);
    constraints.gridy++;

    // Use advanced features
    chkAdvancedFeatures = Utilities.createJCheckBox(
        GT._("Use advanced features (experimental)"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ADVANCED_FEATURES,
            Configuration.DEFAULT_ADVANCED_FEATURES));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkAdvancedFeatures, constraints);
    constraints.gridy++;

    // Display "WikiCleaner" in the update comments
    chkWikiInComments = Utilities.createJCheckBox(
        GT._("Display WikiCleaner link in update comments"),
        configuration.getBoolean(
            Configuration.BOOLEAN_WIKICLEANER_COMMENT,
            Configuration.DEFAULT_WIKICLEANER_COMMENT));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkWikiInComments, constraints);
    constraints.gridy++;

    // Show 0 errors in Check Wiki
    chkShow0ErrorsCheckWiki = Utilities.createJCheckBox(
        GT._("Show errors with no detection found"),
        configuration.getBoolean(
            Configuration.BOOLEAN_CHECK_SHOW_0_ERRORS,
            Configuration.DEFAULT_CHECK_SHOW_0_ERRORS));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkShow0ErrorsCheckWiki, constraints);
    constraints.gridy++;

    // Link to error description in comment for Check Wiki
    chkLinkErrorsCheckWiki = Utilities.createJCheckBox(
        GT._("Add link to error description in comments"),
        configuration.getBoolean(
            Configuration.BOOLEAN_CHECK_LINK_ERRORS,
            Configuration.DEFAULT_CHECK_LINK_ERRORS));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkLinkErrorsCheckWiki, constraints);
    constraints.gridy++;

    // Force watching pages that have been edited
    chkForceWatch = Utilities.createJCheckBox(
        GT._("Watch all edited pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_FORCE_WATCH,
            Configuration.DEFAULT_FORCE_WATCH));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(chkForceWatch, constraints);
    constraints.gridy++;

    // Menu size
    modelMenuSize = new SpinnerNumberModel(
        configuration.getInt(Configuration.INTEGER_MENU_SIZE, Configuration.DEFAULT_MENU_SIZE),
        2, 999, 1);
    spinMenuSize = new JSpinner(modelMenuSize);
    JLabel labelMenuSize = Utilities.createJLabel(GT._("Maximum number of items in a menu :"));
    labelMenuSize.setLabelFor(spinMenuSize);
    labelMenuSize.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMenuSize, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    panel.add(spinMenuSize, constraints);
    constraints.gridy++;

    // Max pages
    modelMaxPages = new SpinnerNumberModel(
        configuration.getInt(Configuration.INTEGER_MAXIMUM_PAGES, Configuration.DEFAULT_MAXIMUM_PAGES),
        1, 99, 1);
    spinMaxPages = new JSpinner(modelMaxPages);
    JLabel labelMaxPages = Utilities.createJLabel(GT._("Maximum number of simultaneous analysis :"));
    labelMaxPages.setLabelFor(spinMaxPages);
    labelMaxPages.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMaxPages, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    panel.add(spinMaxPages, constraints);
    constraints.gridy++;

    // Max errors for Check Wiki
    modelMaxErrorsCheckWiki = new SpinnerNumberModel(
        configuration.getInt(Configuration.INTEGER_CHECK_NB_ERRORS, Configuration.DEFAULT_CHECK_NB_ERRORS),
        10, 1000, 5);
    spinMaxErrorsCheckWiki = new JSpinner(modelMaxErrorsCheckWiki);
    JLabel labelMaxErrorsCheckWiki = Utilities.createJLabel(
        GT._("Maximum number of errors for Check Wiki :"));
    labelMaxErrorsCheckWiki.setLabelFor(spinMaxErrorsCheckWiki);
    labelMaxErrorsCheckWiki.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMaxErrorsCheckWiki, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    panel.add(spinMaxErrorsCheckWiki, constraints);
    constraints.gridy++;

    // Interrogation threads
    modelInterrogationThreads = new SpinnerNumberModel(
        configuration.getInt(Configuration.INTEGER_INTERROG_THREAD, Configuration.DEFAULT_INTERROG_THREAD),
        1, 99, 1);
    spinInterrogationThreads = new JSpinner(modelInterrogationThreads);
    JLabel labelThreads = Utilities.createJLabel(GT._("Maximum number of interrogation threads :"));
    labelThreads.setLabelFor(spinInterrogationThreads);
    labelThreads.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelThreads, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    panel.add(spinInterrogationThreads, constraints);
    constraints.gridy++;

    // Signature
    txtSignature = new JTextField(15);
    txtSignature.setText(configuration.getString(
        Configuration.STRING_SIGNATURE,
        Configuration.DEFAULT_SIGNATURE));
    JLabel labelSignature = Utilities.createJLabel(GT._("Signature :"));
    labelSignature.setLabelFor(txtSignature);
    labelSignature.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelSignature, constraints);
    constraints.gridwidth = 2;
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(txtSignature, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 3;
    constraints.gridx = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);

    return panel;
  }

  /**
   * @return Analysis Window options components.
   */
  private Component createAnalysisComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Full Analysis window options")));
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    constraints.gridwidth = 3;

    // Close full analysis window after sending
    chkCloseFull = Utilities.createJCheckBox(
        GT._("Close full analysis window after sending"),
        configuration.getBoolean(
            Configuration.BOOLEAN_CLOSE_FULL,
            Configuration.DEFAULT_CLOSE_FULL));
    panel.add(chkCloseFull, constraints);
    constraints.gridy++;

    // Create disambiguation warning in main namespace
    chkAnalysisCreateDabWarning = Utilities.createJCheckBox(
        GT._("Create disambiguation warning on talk page (in main namespace)"),
        configuration.getBoolean(
            Configuration.BOOLEAN_CREATE_DAB_WARNING,
            Configuration.DEFAULT_CREATE_DAB_WARNING));
    panel.add(chkAnalysisCreateDabWarning, constraints);
    constraints.gridy++;

    // Create disambiguation warning in other namespace
    chkAnalysisCreateDabWarningAll = Utilities.createJCheckBox(
        GT._("Create disambiguation warning on talk page (in other namespaces)"),
        configuration.getBoolean(
            Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL,
            Configuration.DEFAULT_CREATE_DAB_WARNING_ALL));
    panel.add(chkAnalysisCreateDabWarningAll, constraints);
    constraints.gridy++;

    // Update disambiguation warning in main namespace
    chkAnalysisUpdateDabWarning = Utilities.createJCheckBox(
        GT._("Update disambiguation warning on talk page (in main namespace)"),
        configuration.getBoolean(
            Configuration.BOOLEAN_UPDATE_DAB_WARNING,
            Configuration.DEFAULT_UPDATE_DAB_WARNING));
    panel.add(chkAnalysisUpdateDabWarning, constraints);
    constraints.gridy++;

    // Update disambiguation warning in other namespace
    chkAnalysisUpdateDabWarningAll = Utilities.createJCheckBox(
        GT._("Update disambiguation warning on talk page (in other namespaces)"),
        configuration.getBoolean(
            Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL,
            Configuration.DEFAULT_UPDATE_DAB_WARNING_ALL));
    panel.add(chkAnalysisUpdateDabWarningAll, constraints);
    constraints.gridy++;

    // Show Disambiguation pages
    chkAnalysisDisambig = Utilities.createJCheckBox(
        GT._("Show &disambiguation pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
            Configuration.DEFAULT_ANALYSIS_DISAMBIG_PAGES));
    panel.add(chkAnalysisDisambig, constraints);
    constraints.gridy++;

    // Show Missing pages
    chkAnalysisMissing = Utilities.createJCheckBox(
        GT._("Show &missing pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
            Configuration.DEFAULT_ANALYSIS_MISSING_PAGES));
    panel.add(chkAnalysisMissing, constraints);
    constraints.gridy++;

    // Show Redirect pages
    chkAnalysisRedirect = Utilities.createJCheckBox(
        GT._("Show &redirect pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
            Configuration.DEFAULT_ANALYSIS_REDIRECT_PAGES));
    panel.add(chkAnalysisRedirect, constraints);
    constraints.gridy++;

    // Show Other pages
    chkAnalysisOther = Utilities.createJCheckBox(
        GT._("Show &other pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
            Configuration.DEFAULT_ANALYSIS_OTHER_PAGES));
    panel.add(chkAnalysisOther, constraints);
    constraints.gridy++;

    // Count Disambiguation pages
    chkAnalysisCountDisambig = Utilities.createJCheckBox(
        GT._("Count &disambiguation pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
            Configuration.DEFAULT_ANALYSIS_COUNT_DISAMBIG));
    panel.add(chkAnalysisCountDisambig, constraints);
    constraints.gridy++;

    // Count Missing pages
    chkAnalysisCountMissing = Utilities.createJCheckBox(
        GT._("Count &missing pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
            Configuration.DEFAULT_ANALYSIS_COUNT_MISSING));
    panel.add(chkAnalysisCountMissing, constraints);
    constraints.gridy++;

    // Count Redirect pages
    chkAnalysisCountRedirect = Utilities.createJCheckBox(
        GT._("Count &redirect pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
            Configuration.DEFAULT_ANALYSIS_COUNT_REDIRECT));
    panel.add(chkAnalysisCountRedirect, constraints);
    constraints.gridy++;

    // Count Other pages
    chkAnalysisCountOther = Utilities.createJCheckBox(
        GT._("Count &other pages"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
            Configuration.DEFAULT_ANALYSIS_COUNT_OTHER));
    panel.add(chkAnalysisCountOther, constraints);
    constraints.gridy++;

    // Hide when sending
    chkAnalysisHideSending = Utilities.createJCheckBox(
        GT._("&Hide window when sending"),
        configuration.getBoolean(
            Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING,
            Configuration.DEFAULT_ANALYSIS_HIDE_SENDING));
    panel.add(chkAnalysisHideSending, constraints);
    constraints.gridy++;

    // Save last replacement
    chkSaveLastReplacement = Utilities.createJCheckBox(
        GT._("Save last replacement used"),
        configuration.getBoolean(
            Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT,
            Configuration.DEFAULT_SAVE_LAST_REPLACEMENT));
    panel.add(chkSaveLastReplacement, constraints);
    constraints.gridy++;

    // Remember last page
    chkRememberLastPage = Utilities.createJCheckBox(
        GT._("Remember last edited page"),
        configuration.getBoolean(
            Configuration.BOOLEAN_REMEMBER_LAST_PAGE,
            Configuration.DEFAULT_REMEMBER_LAST_PAGE));
    panel.add(chkRememberLastPage, constraints);
    constraints.gridy++;

    // Nb pages selected
    modelAnalysisNbPages = new SpinnerNumberModel(
        configuration.getInt(Configuration.INTEGER_ANALYSIS_NB_PAGES, Configuration.DEFAULT_ANALYSIS_NB_PAGES),
        1, 99, 1);
    spinAnalysisNbPages = new JSpinner(modelAnalysisNbPages);
    JLabel labelNbPages = Utilities.createJLabel(GT._("Number of links selected :"));
    labelNbPages.setLabelFor(spinAnalysisNbPages);
    labelNbPages.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelNbPages, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    panel.add(spinAnalysisNbPages, constraints);
    constraints.gridy++;

    // Undo levels
    modelAnalysisUndoLevels = new SpinnerNumberModel(
        configuration.getInt(Configuration.INTEGER_ANALYSIS_UNDO_LVL, Configuration.DEFAULT_ANALYSIS_UNDO_LVL),
        0, 99, 1);
    spinAnalysisUndoLevels = new JSpinner(modelAnalysisUndoLevels);
    JLabel labelUndoLevels = Utilities.createJLabel(GT._("Undo levels :"));
    labelUndoLevels.setLabelFor(spinAnalysisUndoLevels);
    labelUndoLevels.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelUndoLevels, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    panel.add(spinAnalysisUndoLevels, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);

    return panel;
  }

  /**
   * @return Sorting options components.
   */
  private Component createSortingComponents() {
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Sort orders
    JPanel panelSortOrders = new JPanel(new GridBagLayout());
    panelSortOrders.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Sort orders")));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    modelSort = new DefaultListModel();
    List<CompositeComparator<Page>> comparators = PageComparator.getComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      modelSort.addElement(comparator);
    }
    listSort = new JList(modelSort);
    listSort.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listSort.addListSelectionListener(this);
    JScrollPane scrollSort = new JScrollPane(listSort);
    scrollSort.setMinimumSize(new Dimension(100, 100));
    scrollSort.setPreferredSize(new Dimension(150, 200));
    scrollSort.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panelSortOrders.add(scrollSort, constraints);
    JToolBar toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    buttonSortAdd = Utilities.createJButton(
        "gnome-list-add.png", EnumImageSize.NORMAL, GT._("Add"), false);
    buttonSortAdd.setActionCommand(ACTION_SORT_ADD);
    buttonSortAdd.addActionListener(this);
    toolbarButtons.add(buttonSortAdd);
    buttonSortDelete = Utilities.createJButton(
        "gnome-list-remove.png", EnumImageSize.NORMAL, GT._("Delete"), false);
    buttonSortDelete.setActionCommand(ACTION_SORT_DELETE);
    buttonSortDelete.addActionListener(this);
    toolbarButtons.add(buttonSortDelete);
    constraints.gridy++;
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panelSortOrders.add(toolbarButtons, constraints);
    constraints.gridy++;

    // Sort description
    JPanel panelSortDescription = new JPanel(new GridBagLayout());
    panelSortDescription.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Details")));
    constraints.gridy = 0;
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    modelSortItem = new DefaultListModel();
    listSortItem = new JList(modelSortItem);
    listSortItem.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane scrollSortItem = new JScrollPane(listSortItem);
    scrollSortItem.setMinimumSize(new Dimension(100, 100));
    scrollSortItem.setPreferredSize(new Dimension(150, 200));
    scrollSortItem.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panelSortDescription.add(scrollSortItem, constraints);
    constraints.gridy++;
    toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    buttonSortUp = Utilities.createJButton(
        "gnome-go-up.png", EnumImageSize.NORMAL, GT._("Up"), false);
    buttonSortUp.setActionCommand(ACTION_SORT_UP);
    buttonSortUp.addActionListener(this);
    toolbarButtons.add(buttonSortUp);
    buttonSortDown = Utilities.createJButton(
        "gnome-go-down.png", EnumImageSize.NORMAL, GT._("Down"), false);
    buttonSortDown.setActionCommand(ACTION_SORT_DOWN);
    buttonSortDown.addActionListener(this);
    toolbarButtons.add(buttonSortDown);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panelSortDescription.add(toolbarButtons, constraints);
    constraints.gridy++;
    if (modelSort.getSize() > 0) {
      listSort.setSelectedIndex(0);
    }

    // Adding panels
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(panelSortOrders, constraints);
    constraints.gridx++;
    panel.add(panelSortDescription, constraints);

    return panel;
  }

  /**
   * @return Debug options components.
   */
  private Component createDebugComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Debug options")));
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    constraints.gridwidth = 3;

    // Debug URL
    chkDebugURL = Utilities.createJCheckBox(
        GT._("Log all URL called by WikiCleaner"),
        configuration.getBoolean(
            Configuration.BOOLEAN_DEBUG_URL,
            Configuration.DEFAULT_DEBUG_URL));
    panel.add(chkDebugURL, constraints);
    constraints.gridy++;

    // Debug XML
    chkDebugXML = Utilities.createJCheckBox(
        GT._("Log all answers to MediaWiki API calls"),
        configuration.getBoolean(
            Configuration.BOOLEAN_DEBUG_XML,
            Configuration.DEFAULT_DEBUG_XML));
    panel.add(chkDebugXML, constraints);
    constraints.gridy++;

    // Debug time
    chkDebugTime = Utilities.createJCheckBox(
        GT._("Add a timestamp to logs"),
        configuration.getBoolean(
            Configuration.BOOLEAN_DEBUG_TIME,
            Configuration.DEFAULT_DEBUG_TIME));
    panel.add(chkDebugTime, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);

    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));

    // Apply button
    buttonApply = Utilities.createJButton(GT._("&Apply"));
    buttonApply.setActionCommand(ACTION_APPLY);
    buttonApply.addActionListener(this);
    panel.add(buttonApply);

    // Validate button
    buttonValidate = Utilities.createJButton(GT._("&Validate"));
    buttonValidate.setActionCommand(ACTION_VALIDATE);
    buttonValidate.addActionListener(this);
    panel.add(buttonValidate);

    // Cancel button
    buttonCancel = Utilities.createJButton(GT._("&Cancel"));
    buttonCancel.setActionCommand(ACTION_CANCEL);
    buttonCancel.addActionListener(this);
    panel.add(buttonCancel);

    // Restore defaults button
    buttonDefault = Utilities.createJButton(GT._("&Restore defaults"));
    buttonDefault.setActionCommand(ACTION_DEFAULT);
    buttonDefault.addActionListener(this);
    panel.add(buttonDefault);

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

    if (ACTION_APPLY.equals(e.getActionCommand())) {
      actionApply();
    } else if (ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate();
    } else if (ACTION_CANCEL.equals(e.getActionCommand())) {
      actionCancel();
    } else if (ACTION_DEFAULT.equals(e.getActionCommand())) {
      actionDefault();
    } else if (ACTION_SORT_ADD.equals(e.getActionCommand())) {
      actionSortAdd();
    } else if (ACTION_SORT_DELETE.equals(e.getActionCommand())) {
      actionSortDelete();
    } else if (ACTION_SORT_DOWN.equals(e.getActionCommand())) {
      actionSortMove(false);
    } else if (ACTION_SORT_UP.equals(e.getActionCommand())) {
      actionSortMove(true);
    }
  }

  /**
   * Action called when Sort Add button is pressed.
   */
  private void actionSortAdd() {
    String name = Utilities.askForValue(getParentComponent(), "Input name :", "", null);
    if (name != null) {
      CompositeComparator<Page> comparator = PageComparator.createComparator(name);
      modelSort.addElement(comparator);
      listSort.setSelectedIndex(modelSort.size() - 1);
    }
  }

  /**
   * Action called when Sort Delete button is pressed.
   */
  private void actionSortDelete() {
    int selected = listSort.getSelectedIndex();
    if (selected != -1) {
      modelSort.remove(selected);
      if (selected < modelSort.size()) {
        listSort.setSelectedIndex(selected);
      } else if (selected > 0) {
        listSort.setSelectedIndex(selected - 1);
      }
    }
  }

  /**
   * Action called when Sort Up / Down button is pressed.
   * @param up Flag indicating if it's the Up button.
   */
  private void actionSortMove(boolean up) {
    Object selectedSort = listSort.getSelectedValue();
    Object selectedItem = listSortItem.getSelectedValue();
    if ((selectedSort instanceof CompositeComparator) &&
        (selectedItem instanceof NamedComparator)) {
      CompositeComparator comparators = (CompositeComparator) selectedSort;
      NamedComparator comparator = (NamedComparator) selectedItem;
      comparators.moveComparator(comparator.getName(), up);
      int selected = listSortItem.getSelectedIndex();
      selected += up ? -1 : 1;
      modelSortItem.clear();
      for (int i = 0; i < comparators.getComparatorsCount(); i++) {
        NamedComparator item = comparators.getComparator(i);
        modelSortItem.addElement(item);
      }
      listSortItem.setSelectedIndex(Math.min(Math.max(0, selected), modelSortItem.size() - 1));
    }
  }

  /**
   * Action called when Apply button is pressed.
   */
  @SuppressWarnings("unchecked")
  private void actionApply() {
    Configuration config = Configuration.getConfiguration();

    // Boolean values
    config.setBoolean(Configuration.BOOLEAN_ADVANCED_FEATURES, chkAdvancedFeatures.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG, chkAnalysisCountDisambig.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING, chkAnalysisCountMissing.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER, chkAnalysisCountOther.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT, chkAnalysisCountRedirect.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES, chkAnalysisDisambig.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING, chkAnalysisHideSending.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES, chkAnalysisMissing.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES, chkAnalysisOther.isSelected());
    config.setBoolean(Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES, chkAnalysisRedirect.isSelected());
    config.setBoolean(Configuration.BOOLEAN_CHECK_SHOW_0_ERRORS, chkShow0ErrorsCheckWiki.isSelected());
    config.setBoolean(Configuration.BOOLEAN_CHECK_LINK_ERRORS, chkLinkErrorsCheckWiki.isSelected());
    config.setBoolean(Configuration.BOOLEAN_CLOSE_DISAMBIG, chkCloseDisambig.isSelected());
    config.setBoolean(Configuration.BOOLEAN_CLOSE_FULL, chkCloseFull.isSelected());
    config.setBoolean(Configuration.BOOLEAN_CREATE_DAB_WARNING, chkAnalysisCreateDabWarning.isSelected());
    config.setBoolean(Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL, chkAnalysisCreateDabWarningAll.isSelected());
    config.setBoolean(Configuration.BOOLEAN_FORCE_WATCH, chkForceWatch.isSelected());
    config.setBoolean(Configuration.BOOLEAN_REMEMBER_LAST_PAGE, chkRememberLastPage.isSelected());
    config.setBoolean(Configuration.BOOLEAN_RESTORE_WINDOW, chkRestoreWindowPosition.isSelected());
    config.setBoolean(Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT, chkSaveLastReplacement.isSelected());
    config.setBoolean(Configuration.BOOLEAN_SAVE_WINDOW, chkSaveWindowPosition.isSelected());
    config.setBoolean(Configuration.BOOLEAN_SHORT_NOTATION, chkShortNotation.isSelected());
    config.setBoolean(Configuration.BOOLEAN_UPDATE_DAB_WARNING, chkAnalysisUpdateDabWarning.isSelected());
    config.setBoolean(Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL, chkAnalysisUpdateDabWarningAll.isSelected());
    config.setBoolean(Configuration.BOOLEAN_WIKICLEANER_COMMENT, chkWikiInComments.isSelected());

    // Integer values
    config.setInt(Configuration.INTEGER_ANALYSIS_NB_PAGES, modelAnalysisNbPages.getNumber().intValue());
    config.setInt(Configuration.INTEGER_CHECK_NB_ERRORS, modelMaxErrorsCheckWiki.getNumber().intValue());
    config.setInt(Configuration.INTEGER_INTERROG_THREAD, modelInterrogationThreads.getNumber().intValue());
    config.setInt(Configuration.INTEGER_MENU_SIZE, modelMenuSize.getNumber().intValue());
    config.setInt(Configuration.INTEGER_MAXIMUM_PAGES, modelMaxPages.getNumber().intValue());

    // String values
    config.setString(Configuration.STRING_SIGNATURE, txtSignature.getText());

    // Sorting orders
    List<CompositeComparator<Page>> comparators = new LinkedList<CompositeComparator<Page>>();
    for (int i = modelSort.getSize(); i > 0; i--) {
      Object sort = modelSort.get(i - 1);
      if (sort instanceof CompositeComparator) {
        comparators.add(0, (CompositeComparator<Page>) sort);
      }
    }
    PageComparator.setComparators(comparators);

    config.updateConfiguration();
  }

  /**
   * Action called when Default button is pressed.
   */
  private void actionDefault() {
    Configuration config = Configuration.getConfiguration();

    // Boolean values
    chkAdvancedFeatures.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ADVANCED_FEATURES,
        Configuration.DEFAULT_ADVANCED_FEATURES));
    chkAnalysisCountDisambig.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
        Configuration.DEFAULT_ANALYSIS_COUNT_DISAMBIG));
    chkAnalysisCountMissing.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
        Configuration.DEFAULT_ANALYSIS_COUNT_MISSING));
    chkAnalysisCountOther.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
        Configuration.DEFAULT_ANALYSIS_COUNT_OTHER));
    chkAnalysisCountRedirect.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
        Configuration.DEFAULT_ANALYSIS_COUNT_REDIRECT));
    chkAnalysisCreateDabWarning.setSelected(config.getBoolean(
        Configuration.BOOLEAN_CREATE_DAB_WARNING,
        Configuration.DEFAULT_CREATE_DAB_WARNING));
    chkAnalysisCreateDabWarningAll.setSelected(config.getBoolean(
        Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL,
        Configuration.DEFAULT_CREATE_DAB_WARNING_ALL));
    chkAnalysisDisambig.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
        Configuration.DEFAULT_ANALYSIS_DISAMBIG_PAGES));
    chkAnalysisHideSending.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING,
        Configuration.DEFAULT_ANALYSIS_HIDE_SENDING));
    chkAnalysisMissing.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
        Configuration.DEFAULT_ANALYSIS_MISSING_PAGES));
    chkAnalysisOther.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
        Configuration.DEFAULT_ANALYSIS_OTHER_PAGES));
    chkAnalysisRedirect.setSelected(config.getBoolean(
        Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
        Configuration.DEFAULT_ANALYSIS_REDIRECT_PAGES));
    chkAnalysisUpdateDabWarning.setSelected(config.getBoolean(
        Configuration.BOOLEAN_UPDATE_DAB_WARNING,
        Configuration.DEFAULT_UPDATE_DAB_WARNING));
    chkAnalysisUpdateDabWarningAll.setSelected(config.getBoolean(
        Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL,
        Configuration.DEFAULT_UPDATE_DAB_WARNING_ALL));
    chkShow0ErrorsCheckWiki.setSelected(config.getBoolean(
        Configuration.BOOLEAN_CHECK_SHOW_0_ERRORS,
        Configuration.DEFAULT_CHECK_SHOW_0_ERRORS));
    chkLinkErrorsCheckWiki.setSelected(config.getBoolean(
        Configuration.BOOLEAN_CHECK_LINK_ERRORS,
        Configuration.DEFAULT_CHECK_LINK_ERRORS));
    chkCloseDisambig.setSelected(config.getBoolean(
        Configuration.BOOLEAN_CLOSE_DISAMBIG,
        Configuration.DEFAULT_CLOSE_DISAMBIG));
    chkCloseFull.setSelected(config.getBoolean(
        Configuration.BOOLEAN_CLOSE_FULL,
        Configuration.DEFAULT_CLOSE_FULL));
    chkForceWatch.setSelected(config.getBoolean(
        Configuration.BOOLEAN_FORCE_WATCH,
        Configuration.DEFAULT_FORCE_WATCH));
    chkRememberLastPage.setSelected(config.getBoolean(
        Configuration.BOOLEAN_REMEMBER_LAST_PAGE,
        Configuration.DEFAULT_REMEMBER_LAST_PAGE));
    chkRestoreWindowPosition.setSelected(config.getBoolean(
        Configuration.BOOLEAN_RESTORE_WINDOW,
        Configuration.DEFAULT_RESTORE_WINDOW));
    chkSaveLastReplacement.setSelected(config.getBoolean(
        Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT,
        Configuration.DEFAULT_SAVE_LAST_REPLACEMENT));
    chkSaveWindowPosition.setSelected(config.getBoolean(
        Configuration.BOOLEAN_SAVE_WINDOW,
        Configuration.DEFAULT_SAVE_WINDOW));
    chkShortNotation.setSelected(config.getBoolean(
        Configuration.BOOLEAN_SHORT_NOTATION,
        Configuration.DEFAULT_SHORT_NOTATION));

    // Integer values
    modelAnalysisNbPages.setValue(config.getInt(
        Configuration.INTEGER_ANALYSIS_NB_PAGES,
        Configuration.DEFAULT_ANALYSIS_NB_PAGES));
    modelMaxErrorsCheckWiki.setValue(config.getInt(
        Configuration.INTEGER_CHECK_NB_ERRORS,
        Configuration.DEFAULT_CHECK_NB_ERRORS));
    modelInterrogationThreads.setValue(config.getInt(
        Configuration.INTEGER_INTERROG_THREAD,
        Configuration.DEFAULT_INTERROG_THREAD));
    modelMenuSize.setValue(config.getInt(
        Configuration.INTEGER_MENU_SIZE,
        Configuration.DEFAULT_MENU_SIZE));
    modelMaxPages.setValue(config.getInt(
        Configuration.INTEGER_MAXIMUM_PAGES,
        Configuration.DEFAULT_MAXIMUM_PAGES));

    // String values
    txtSignature.setText(config.getString(
        Configuration.STRING_SIGNATURE,
        Configuration.DEFAULT_SIGNATURE));

    // Sorting orders
    modelSort.clear();
    List<CompositeComparator<Page>> comparators = PageComparator.getDefaultComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      modelSort.addElement(comparator);
    }
  }

  /**
   * Action called when Validate button is pressed.
   */
  private void actionValidate() {
    actionApply();
    dispose();
  }

  /**
   * Action called when Cancel button is pressed.
   */
  private void actionCancel() {
    dispose();
  }

  /* ====================================================================== */
  /* ListSelectionListener implementation                                   */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
   */
  @SuppressWarnings("unchecked")
  public void valueChanged(ListSelectionEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    if (e.getSource() == listSort) {
      Object value = listSort.getSelectedValue();
      if (value instanceof CompositeComparator) {
        modelSortItem.clear();
        CompositeComparator<Page> comparator = (CompositeComparator<Page>) value;
        for (int i = 0; i < comparator.getComparatorsCount(); i++) {
          NamedComparator<Page> item = comparator.getComparator(i);
          modelSortItem.addElement(item);
        }
      }
    }
  }
}
