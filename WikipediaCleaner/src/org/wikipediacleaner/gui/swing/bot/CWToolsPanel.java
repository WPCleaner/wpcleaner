/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * A panel for Check Wiki bot tools.
 */
public class CWToolsPanel extends BotToolsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -8468890591813838957L;

  /**
   * Button for running automatic fixing of Check Wiki errors.
   */
  private JButton buttonCWAutomaticFixing;

  /**
   * Button for marking already fixed Check Wiki errors.
   */
  private JButton buttonCWAutomaticMarking;

  /**
   * Button for checking white lists.
   */
  private JButton buttonCWCheckWhiteLists;

  /**
   * Configure if page that couldn't be fixed should be analyzed.
   */
  private JCheckBox chkCWAnalyze;

  /**
   * Configure number of pages for each error.
   */
  private SpinnerNumberModel modelNbPages;

  /**
   * Comment.
   */
  private JTextField txtComment;

  /**
   * Table model for Check Wiki automatic fixing.
   */
  BotCWTableModel modelCWAutomaticFixing;

  /**
   * Table for Check Wiki automatic fixing.
   */
  private JTable tableCWAutomaticFixing;

  /**
   * Construct a Check Wiki bot tools panel.
   * 
   * @param window Parent window.
   */
  public CWToolsPanel(BasicWindow window) {
    super(window, new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {

    // Configuration
    Configuration config = Configuration.getConfiguration();

    // Initialize constraints
    final int maxX = 3;
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = maxX;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Label
    JLabel labelCW = Utilities.createJLabel(GT._("Automatic fixing for Check Wiki"));
    add(labelCW, constraints);
    constraints.gridy++;

    // Table for listing errors
    modelCWAutomaticFixing = new BotCWTableModel(window.getWikipedia());
    tableCWAutomaticFixing = new JTable(modelCWAutomaticFixing);
    tableCWAutomaticFixing.setRowSelectionAllowed(false);
    tableCWAutomaticFixing.setColumnSelectionAllowed(false);
    modelCWAutomaticFixing.configureColumnModel(tableCWAutomaticFixing);
    Utilities.addRowSorter(tableCWAutomaticFixing, modelCWAutomaticFixing);
    JScrollPane paneCWAutomaticFixing = new JScrollPane(tableCWAutomaticFixing);
    paneCWAutomaticFixing.setMinimumSize(new Dimension(200, 200));
    paneCWAutomaticFixing.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
    paneCWAutomaticFixing.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.weighty = 1;
    add(paneCWAutomaticFixing, constraints);
    constraints.gridy++;
    constraints.weighty = 0;

    // Check box to decide if pages should be analyzed if not fixed
    chkCWAnalyze = Utilities.createJCheckBox(
        GT._("Analyze pages that couldn't be fixed by bot"),
        config.getBoolean(null, ConfigurationValueBoolean.CHECK_BOT_ANALYZE));
    add(chkCWAnalyze, constraints);
    constraints.gridy++;

    // Number of errors
    modelNbPages = new SpinnerNumberModel(
        config.getInt(null, ConfigurationValueInteger.CHECK_BOT_NB_PAGES), 1, 10000, 1);
    JSpinner spinNbPages = new JSpinner(modelNbPages);
    JLabel labelNbPages = Utilities.createJLabel(GT._("Number of pages:"));
    labelNbPages.setLabelFor(spinNbPages);
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    add(labelNbPages, constraints);
    constraints.gridx += constraints.gridwidth;
    constraints.gridwidth = maxX - constraints.gridwidth;
    constraints.weightx = 0;
    add(spinNbPages, constraints);
    constraints.gridx = 0;
    constraints.gridy++;

    // Comment
    txtComment = Utilities.createJTextField("", 40);
    JLabel labelComment = Utilities.createJLabel(GT._("Comment:"));
    labelComment.setLabelFor(txtComment);
    constraints.gridwidth = 1;
    constraints.weightx = 0;
    add(labelComment, constraints);
    constraints.gridx += constraints.gridwidth;
    constraints.gridwidth = maxX - constraints.gridwidth;
    constraints.weightx = 1;
    add(txtComment, constraints);
    constraints.gridx = 0;
    constraints.gridy++;

    // Button for running the automatic fixing
    buttonCWAutomaticFixing = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Automatic fixing for Check Wiki"), true, null);
    buttonCWAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCWAutomaticFixing"));
    constraints.gridwidth = maxX;
    constraints.weightx = 1;
    add(buttonCWAutomaticFixing, constraints);
    constraints.gridy++;

    // Button for marking errors already fixed
    buttonCWAutomaticMarking = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Mark errors already fixed"), true, null);
    buttonCWAutomaticMarking.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCWAutomaticMarking"));
    constraints.gridwidth = maxX;
    constraints.weightx = 1;
    add(buttonCWAutomaticMarking, constraints);
    constraints.gridy++;

    // Button for checking white lists
    buttonCWCheckWhiteLists = Utilities.createJButton(
        "gnome-edit-find.png", EnumImageSize.NORMAL,
        GT._("Check whitelists"), true, null);
    buttonCWCheckWhiteLists.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCWCheckWhiteLists"));
    constraints.gridwidth = maxX;
    constraints.weightx = 1;
    add(buttonCWCheckWhiteLists, constraints);
    constraints.gridy++;

    // Select errors that can be fixed by bot
    ListSelectionModel selectionModel = tableCWAutomaticFixing.getSelectionModel();
    selectionModel.clearSelection();
  }

  /**
   * Update components state.
   */
  @Override
  protected void updateComponentState() {
    buttonCWAutomaticFixing.setEnabled(true);
  }

  /**
   * Action called when Automatic Check Wiki Fixing button is pressed.
   */
  public void actionCWAutomaticFixing() {
    automaticFixing(true);
  }

  /**
   * Action called when Automatic Check Wiki Marking button is pressed.
   */
  public void actionCWAutomaticMarking() {
    automaticFixing(false);
  }

  /**
   * @param saveModifications True if modifications should be saved.
   */
  private void automaticFixing(boolean saveModifications) {
    EnumWikipedia wiki = window.getWikipedia();
    if (!wiki.getCWConfiguration().isProjectAvailable()) {
      Utilities.displayMissingConfiguration(
          window.getParentComponent(), null);
      return;
    }
    List<CheckErrorAlgorithm> fixAlgorithms = modelCWAutomaticFixing.getFixAlgorithms();
    if ((fixAlgorithms == null) || fixAlgorithms.isEmpty()) {
      window.displayWarning(GT._("You must select at least one algorithm for fixing errors"));
      return;
    }
    List<CheckErrorAlgorithm> listAlgorithms = modelCWAutomaticFixing.getListAlgorithms();
    if ((listAlgorithms == null) || listAlgorithms.isEmpty()) {
      window.displayWarning(GT._("You must select at least one algorithm for listing errors"));
      return;
    }
    int answer = window.displayYesNoWarning(BasicWindow.experimentalMessage);
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    int max = 100;
    if (modelNbPages.getNumber() != null) {
      Number number = modelNbPages.getNumber();
      max = number.intValue();
    }
    Configuration config = Configuration.getConfiguration();
    boolean analyze = false;
    if (saveModifications) {
      analyze = chkCWAnalyze.isSelected();
      config.setBoolean(null, ConfigurationValueBoolean.CHECK_BOT_ANALYZE, analyze);
    }
    config.setInt(null, ConfigurationValueInteger.CHECK_BOT_NB_PAGES, max);
    AutomaticCWWorker worker = new AutomaticCWWorker(
        wiki, window,
        listAlgorithms, max,
        fixAlgorithms,
        txtComment.getText(),
        saveModifications, analyze);
    worker.start();
  }

  /**
   * Action called when Check White Lists button is pressed.
   */
  public void actionCWCheckWhiteLists() {
    EnumWikipedia wiki = window.getWikipedia();
    CWCheckWhiteListsWorker worker = new CWCheckWhiteListsWorker(wiki, window);
    worker.start();
  }
}
