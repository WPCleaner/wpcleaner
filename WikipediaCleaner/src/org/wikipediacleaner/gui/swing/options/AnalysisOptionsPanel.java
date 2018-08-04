/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.options;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * A panel for analysis options.
 */
public class AnalysisOptionsPanel extends OptionsPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = 8159287537211853955L;

  /**
   * Construct a General Options panel. 
   */
  public AnalysisOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("Full Analysis window options")));
    JCheckBox chk = null;
    JSpinner spin = null;

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

    constraints.gridwidth = 3;

    // Close full analysis window after sending
    chk = createJCheckBox(
        GT._T("Close full analysis window after sending"),
        ConfigurationValueBoolean.CLOSE_FULL);
    add(chk, constraints);
    constraints.gridy++;

    // Create disambiguation warning in main namespace
    chk = createJCheckBox(
        GT._T("Create disambiguation warning on talk page (in main namespace)"),
        ConfigurationValueBoolean.CREATE_DAB_WARNING);
    add(chk, constraints);
    constraints.gridy++;

    // Create disambiguation warning in encyclopedic namespaces
    chk = createJCheckBox(
        GT._T("Create disambiguation warning on talk page (in encyclopedic namespaces)"),
        ConfigurationValueBoolean.CREATE_DAB_WARNING_ENCY);
    add(chk, constraints);
    constraints.gridy++;

    // Create disambiguation warning in other namespace
    chk = createJCheckBox(
        GT._T("Create disambiguation warning on talk page (in other namespaces)"),
        ConfigurationValueBoolean.CREATE_DAB_WARNING_ALL);
    add(chk, constraints);
    constraints.gridy++;

    // Update disambiguation warning in main namespace
    chk = createJCheckBox(
        GT._T("Update disambiguation warning on talk page (in main namespace)"),
        ConfigurationValueBoolean.UPDATE_DAB_WARNING);
    add(chk, constraints);
    constraints.gridy++;

    // Update disambiguation warning in encyclopedic namespaces
    chk = createJCheckBox(
        GT._T("Update disambiguation warning on talk page (in encyclopedic namespaces)"),
        ConfigurationValueBoolean.UPDATE_DAB_WARNING_ENCY);
    add(chk, constraints);
    constraints.gridy++;

    // Update disambiguation warning in other namespace
    chk = createJCheckBox(
        GT._T("Update disambiguation warning on talk page (in other namespaces)"),
        ConfigurationValueBoolean.UPDATE_DAB_WARNING_ALL);
    add(chk, constraints);
    constraints.gridy++;

    // Show Disambiguation pages
    chk = createJCheckBox(
        GT._T("Show disambiguation pages"),
        ConfigurationValueBoolean.ANALYSIS_DISAMBIG_PAGES);
    add(chk, constraints);
    constraints.gridy++;

    // Show Missing pages
    chk = createJCheckBox(
        GT._T("Show missing pages"),
        ConfigurationValueBoolean.ANALYSIS_MISSING_PAGES);
    add(chk, constraints);
    constraints.gridy++;

    // Show Redirect pages
    chk = createJCheckBox(
        GT._T("Show redirect pages"),
        ConfigurationValueBoolean.ANALYSIS_REDIRECT_PAGES);
    add(chk, constraints);
    constraints.gridy++;

    // Show Other pages
    chk = createJCheckBox(
        GT._T("Show other pages"),
        ConfigurationValueBoolean.ANALYSIS_OTHER_PAGES);
    add(chk, constraints);
    constraints.gridy++;

    // Count Disambiguation pages
    chk = createJCheckBox(
        GT._T("Count disambiguation pages"),
        ConfigurationValueBoolean.ANALYSIS_COUNT_DISAMBIG);
    add(chk, constraints);
    constraints.gridy++;

    // Count Missing pages
    chk = createJCheckBox(
        GT._T("Count missing pages"),
        ConfigurationValueBoolean.ANALYSIS_COUNT_MISSING);
    add(chk, constraints);
    constraints.gridy++;

    // Count Redirect pages
    chk = createJCheckBox(
        GT._T("Count redirect pages"),
        ConfigurationValueBoolean.ANALYSIS_COUNT_REDIRECT);
    add(chk, constraints);
    constraints.gridy++;

    // Count Other pages
    chk = createJCheckBox(
        GT._T("Count other pages"),
        ConfigurationValueBoolean.ANALYSIS_COUNT_OTHER);
    add(chk, constraints);
    constraints.gridy++;

    // Hide when sending
    chk = createJCheckBox(
        GT._T("Hide window when sending"),
        ConfigurationValueBoolean.ANALYSIS_HIDE_SENDING);
    add(chk, constraints);
    constraints.gridy++;

    // Save last replacement
    chk = createJCheckBox(
        GT._T("Save last replacement used"),
        ConfigurationValueBoolean.SAVE_LAST_REPLACEMENT);
    add(chk, constraints);
    constraints.gridy++;

    // Remember last page
    chk = createJCheckBox(
        GT._T("Remember last edited page"),
        ConfigurationValueBoolean.REMEMBER_LAST_PAGE);
    add(chk, constraints);
    constraints.gridy++;

    // Nb pages selected
    spin = createJSpinner(
        ConfigurationValueInteger.ANALYSIS_NB_PAGES,
        1, 99, 1);
    JLabel labelNbPages = Utilities.createJLabel(GT._T("Number of links selected :"));
    labelNbPages.setLabelFor(spin);
    labelNbPages.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelNbPages, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Undo levels
    spin = createJSpinner(
        ConfigurationValueInteger.ANALYSIS_UNDO_LVL,
        0, 99, 1);
    JLabel labelUndoLevels = Utilities.createJLabel(GT._T("Undo levels :"));
    labelUndoLevels.setLabelFor(spin);
    labelUndoLevels.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelUndoLevels, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weighty = 1;
    add(emptyPanel, constraints);
  }
}
