/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A panel for analysis options.
 */
public class AnalysisOptionsPanel extends OptionsPanel {

  /**
   * 
   */
  private static final long serialVersionUID = 8159287537211853955L;

  private JCheckBox chkAnalysisCountDisambig;
  private JCheckBox chkAnalysisCountMissing;
  private JCheckBox chkAnalysisCountOther;
  private JCheckBox chkAnalysisCountRedirect;
  private JCheckBox chkAnalysisCreateDabWarning;
  private JCheckBox chkAnalysisCreateDabWarningEncyclo;
  private JCheckBox chkAnalysisCreateDabWarningAll;
  private JCheckBox chkAnalysisDisambig;
  private JCheckBox chkAnalysisHideSending;
  private JCheckBox chkAnalysisMissing;
  private JCheckBox chkAnalysisOther;
  private JCheckBox chkAnalysisRedirect;
  private JCheckBox chkAnalysisUpdateDabWarning;
  private JCheckBox chkAnalysisUpdateDabWarningEncyclo;
  private JCheckBox chkAnalysisUpdateDabWarningAll;
  private JCheckBox chkCloseFull;
  private JCheckBox chkRememberLastPage;
  private JCheckBox chkSaveLastReplacement;

  private JSpinner spinAnalysisNbPages;
  private SpinnerNumberModel modelAnalysisNbPages;
  private JSpinner spinAnalysisUndoLevels;
  private SpinnerNumberModel modelAnalysisUndoLevels;

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
            null,
            Configuration.BOOLEAN_CLOSE_FULL,
            Configuration.DEFAULT_CLOSE_FULL));
    add(chkCloseFull, constraints);
    constraints.gridy++;

    // Create disambiguation warning in main namespace
    chkAnalysisCreateDabWarning = Utilities.createJCheckBox(
        GT._("Create disambiguation warning on talk page (in main namespace)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_CREATE_DAB_WARNING,
            Configuration.DEFAULT_CREATE_DAB_WARNING));
    add(chkAnalysisCreateDabWarning, constraints);
    constraints.gridy++;

    // Create disambiguation warning in encyclopedic namespaces
    chkAnalysisCreateDabWarningEncyclo = Utilities.createJCheckBox(
        GT._("Create disambiguation warning on talk page (in encyclopedic namespaces)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_CREATE_DAB_WARNING_ENCY,
            Configuration.DEFAULT_CREATE_DAB_WARNING_ENCY));
    add(chkAnalysisCreateDabWarningEncyclo, constraints);
    constraints.gridy++;

    // Create disambiguation warning in other namespace
    chkAnalysisCreateDabWarningAll = Utilities.createJCheckBox(
        GT._("Create disambiguation warning on talk page (in other namespaces)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL,
            Configuration.DEFAULT_CREATE_DAB_WARNING_ALL));
    add(chkAnalysisCreateDabWarningAll, constraints);
    constraints.gridy++;

    // Update disambiguation warning in main namespace
    chkAnalysisUpdateDabWarning = Utilities.createJCheckBox(
        GT._("Update disambiguation warning on talk page (in main namespace)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_UPDATE_DAB_WARNING,
            Configuration.DEFAULT_UPDATE_DAB_WARNING));
    add(chkAnalysisUpdateDabWarning, constraints);
    constraints.gridy++;

    // Update disambiguation warning in encyclopedic namespaces
    chkAnalysisUpdateDabWarningEncyclo = Utilities.createJCheckBox(
        GT._("Update disambiguation warning on talk page (in encyclopedic namespaces)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_UPDATE_DAB_WARNING_ENCY,
            Configuration.DEFAULT_UPDATE_DAB_WARNING_ENCY));
    add(chkAnalysisUpdateDabWarningEncyclo, constraints);
    constraints.gridy++;

    // Update disambiguation warning in other namespace
    chkAnalysisUpdateDabWarningAll = Utilities.createJCheckBox(
        GT._("Update disambiguation warning on talk page (in other namespaces)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL,
            Configuration.DEFAULT_UPDATE_DAB_WARNING_ALL));
    add(chkAnalysisUpdateDabWarningAll, constraints);
    constraints.gridy++;

    // Show Disambiguation pages
    chkAnalysisDisambig = Utilities.createJCheckBox(
        GT._("Show &disambiguation pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
            Configuration.DEFAULT_ANALYSIS_DISAMBIG_PAGES));
    add(chkAnalysisDisambig, constraints);
    constraints.gridy++;

    // Show Missing pages
    chkAnalysisMissing = Utilities.createJCheckBox(
        GT._("Show &missing pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
            Configuration.DEFAULT_ANALYSIS_MISSING_PAGES));
    add(chkAnalysisMissing, constraints);
    constraints.gridy++;

    // Show Redirect pages
    chkAnalysisRedirect = Utilities.createJCheckBox(
        GT._("Show &redirect pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
            Configuration.DEFAULT_ANALYSIS_REDIRECT_PAGES));
    add(chkAnalysisRedirect, constraints);
    constraints.gridy++;

    // Show Other pages
    chkAnalysisOther = Utilities.createJCheckBox(
        GT._("Show &other pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
            Configuration.DEFAULT_ANALYSIS_OTHER_PAGES));
    add(chkAnalysisOther, constraints);
    constraints.gridy++;

    // Count Disambiguation pages
    chkAnalysisCountDisambig = Utilities.createJCheckBox(
        GT._("Count &disambiguation pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
            Configuration.DEFAULT_ANALYSIS_COUNT_DISAMBIG));
    add(chkAnalysisCountDisambig, constraints);
    constraints.gridy++;

    // Count Missing pages
    chkAnalysisCountMissing = Utilities.createJCheckBox(
        GT._("Count &missing pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
            Configuration.DEFAULT_ANALYSIS_COUNT_MISSING));
    add(chkAnalysisCountMissing, constraints);
    constraints.gridy++;

    // Count Redirect pages
    chkAnalysisCountRedirect = Utilities.createJCheckBox(
        GT._("Count &redirect pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
            Configuration.DEFAULT_ANALYSIS_COUNT_REDIRECT));
    add(chkAnalysisCountRedirect, constraints);
    constraints.gridy++;

    // Count Other pages
    chkAnalysisCountOther = Utilities.createJCheckBox(
        GT._("Count &other pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
            Configuration.DEFAULT_ANALYSIS_COUNT_OTHER));
    add(chkAnalysisCountOther, constraints);
    constraints.gridy++;

    // Hide when sending
    chkAnalysisHideSending = Utilities.createJCheckBox(
        GT._("&Hide window when sending"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING,
            Configuration.DEFAULT_ANALYSIS_HIDE_SENDING));
    add(chkAnalysisHideSending, constraints);
    constraints.gridy++;

    // Save last replacement
    chkSaveLastReplacement = Utilities.createJCheckBox(
        GT._("Save last replacement used"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT,
            Configuration.DEFAULT_SAVE_LAST_REPLACEMENT));
    add(chkSaveLastReplacement, constraints);
    constraints.gridy++;

    // Remember last page
    chkRememberLastPage = Utilities.createJCheckBox(
        GT._("Remember last edited page"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_REMEMBER_LAST_PAGE,
            Configuration.DEFAULT_REMEMBER_LAST_PAGE));
    add(chkRememberLastPage, constraints);
    constraints.gridy++;

    // Nb pages selected
    modelAnalysisNbPages = new SpinnerNumberModel(
        configuration.getInt(
            null,
            Configuration.INTEGER_ANALYSIS_NB_PAGES,
            Configuration.DEFAULT_ANALYSIS_NB_PAGES),
        1, 99, 1);
    spinAnalysisNbPages = new JSpinner(modelAnalysisNbPages);
    JLabel labelNbPages = Utilities.createJLabel(GT._("Number of links selected :"));
    labelNbPages.setLabelFor(spinAnalysisNbPages);
    labelNbPages.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelNbPages, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spinAnalysisNbPages, constraints);
    constraints.gridy++;

    // Undo levels
    modelAnalysisUndoLevels = new SpinnerNumberModel(
        configuration.getInt(
            null,
            Configuration.INTEGER_ANALYSIS_UNDO_LVL,
            Configuration.DEFAULT_ANALYSIS_UNDO_LVL),
        0, 99, 1);
    spinAnalysisUndoLevels = new JSpinner(modelAnalysisUndoLevels);
    JLabel labelUndoLevels = Utilities.createJLabel(GT._("Undo levels :"));
    labelUndoLevels.setLabelFor(spinAnalysisUndoLevels);
    labelUndoLevels.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelUndoLevels, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spinAnalysisUndoLevels, constraints);
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

  /**
   * Restore all options to their default values.
   */
  @Override
  public void defaultValues() {
    Configuration config = Configuration.getConfiguration();

    // Boolean values
    chkAnalysisCountDisambig.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
        Configuration.DEFAULT_ANALYSIS_COUNT_DISAMBIG));
    chkAnalysisCountMissing.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
        Configuration.DEFAULT_ANALYSIS_COUNT_MISSING));
    chkAnalysisCountOther.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
        Configuration.DEFAULT_ANALYSIS_COUNT_OTHER));
    chkAnalysisCountRedirect.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
        Configuration.DEFAULT_ANALYSIS_COUNT_REDIRECT));
    chkAnalysisCreateDabWarning.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CREATE_DAB_WARNING,
        Configuration.DEFAULT_CREATE_DAB_WARNING));
    chkAnalysisCreateDabWarningAll.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL,
        Configuration.DEFAULT_CREATE_DAB_WARNING_ALL));
    chkAnalysisCreateDabWarningEncyclo.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CREATE_DAB_WARNING_ENCY,
        Configuration.DEFAULT_CREATE_DAB_WARNING_ENCY));
    chkAnalysisDisambig.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
        Configuration.DEFAULT_ANALYSIS_DISAMBIG_PAGES));
    chkAnalysisHideSending.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING,
        Configuration.DEFAULT_ANALYSIS_HIDE_SENDING));
    chkAnalysisMissing.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
        Configuration.DEFAULT_ANALYSIS_MISSING_PAGES));
    chkAnalysisOther.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
        Configuration.DEFAULT_ANALYSIS_OTHER_PAGES));
    chkAnalysisRedirect.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
        Configuration.DEFAULT_ANALYSIS_REDIRECT_PAGES));
    chkAnalysisUpdateDabWarning.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_UPDATE_DAB_WARNING,
        Configuration.DEFAULT_UPDATE_DAB_WARNING));
    chkAnalysisUpdateDabWarningAll.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL,
        Configuration.DEFAULT_UPDATE_DAB_WARNING_ALL));
    chkAnalysisUpdateDabWarningEncyclo.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_UPDATE_DAB_WARNING_ENCY,
        Configuration.DEFAULT_UPDATE_DAB_WARNING_ENCY));
    chkCloseFull.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CLOSE_FULL,
        Configuration.DEFAULT_CLOSE_FULL));
    chkRememberLastPage.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_REMEMBER_LAST_PAGE,
        Configuration.DEFAULT_REMEMBER_LAST_PAGE));
    chkSaveLastReplacement.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT,
        Configuration.DEFAULT_SAVE_LAST_REPLACEMENT));

    // Integer values
    modelAnalysisNbPages.setValue(config.getInt(
        null,
        Configuration.INTEGER_ANALYSIS_NB_PAGES,
        Configuration.DEFAULT_ANALYSIS_NB_PAGES));
    modelAnalysisUndoLevels.setValue(config.getInt(
        null,
        Configuration.INTEGER_ANALYSIS_UNDO_LVL,
        Configuration.DEFAULT_ANALYSIS_UNDO_LVL));
  }

  /**
   * Apply new values to the options.
   */
  @Override
  public void apply() {
    Configuration config = Configuration.getConfiguration();

    // Boolean values
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
        chkAnalysisCountDisambig.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
        chkAnalysisCountMissing.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
        chkAnalysisCountOther.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
        chkAnalysisCountRedirect.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CREATE_DAB_WARNING,
        chkAnalysisCreateDabWarning.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL,
        chkAnalysisCreateDabWarningAll.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CREATE_DAB_WARNING_ENCY,
        chkAnalysisCreateDabWarningEncyclo.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
        chkAnalysisDisambig.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_HIDE_SENDING,
        chkAnalysisHideSending.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
        chkAnalysisMissing.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
        chkAnalysisOther.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
        chkAnalysisRedirect.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_UPDATE_DAB_WARNING,
        chkAnalysisUpdateDabWarning.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL,
        chkAnalysisUpdateDabWarningAll.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_UPDATE_DAB_WARNING_ENCY,
        chkAnalysisUpdateDabWarningEncyclo.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CLOSE_FULL,
        chkCloseFull.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_REMEMBER_LAST_PAGE,
        chkRememberLastPage.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT,
        chkSaveLastReplacement.isSelected());

    // Integer values
    config.setInt(
        null,
        Configuration.INTEGER_ANALYSIS_NB_PAGES,
        modelAnalysisNbPages.getNumber().intValue());
    config.setInt(
        null,
        Configuration.INTEGER_ANALYSIS_UNDO_LVL,
        modelAnalysisUndoLevels.getNumber().intValue());
  }
}
