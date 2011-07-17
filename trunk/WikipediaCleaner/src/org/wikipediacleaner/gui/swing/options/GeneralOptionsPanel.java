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
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A panel for general options. 
 */
public class GeneralOptionsPanel extends OptionsPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = -7125394212316622303L;

  private JCheckBox chkAdvancedFeatures;
  private JCheckBox chkCloseDisambig;
  private JCheckBox chkForceWatch;
  private JCheckBox chkLinkErrorsCheckWiki;
  private JCheckBox chkOrthograph;
  private JCheckBox chkRestoreWindowPosition;
  private JCheckBox chkSaveWindowPosition;
  private JCheckBox chkShortNotation;
  private JCheckBox chkShow0ErrorsCheckWiki;
  private JCheckBox chkWikiInComments;

  private SpinnerNumberModel modelMaxErrorsCheckWiki;
  private JSpinner spinMaxErrorsCheckWiki;
  private SpinnerNumberModel modelMaxPages;
  private JSpinner spinMaxPages;
  private SpinnerNumberModel modelMenuSize;
  private JSpinner spinMenuSize;
  private SpinnerNumberModel modelInterrogationThreads;
  private JSpinner spinInterrogationThreads;

  private JTextField txtSignature;

  /**
   * Construct a General Options panel. 
   */
  public GeneralOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("General options")));
    Configuration configuration = Configuration.getConfiguration();

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

    constraints.gridwidth = 3;

    // Close disambiguation window after sending
    chkCloseDisambig = Utilities.createJCheckBox(
        GT._("Close disambiguation window after sending"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_CLOSE_DISAMBIG,
            Configuration.DEFAULT_CLOSE_DISAMBIG));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkCloseDisambig, constraints);
    constraints.gridy++;

    // Restore window position
    chkRestoreWindowPosition = Utilities.createJCheckBox(
        GT._("Restore window position"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_RESTORE_WINDOW,
            Configuration.DEFAULT_RESTORE_WINDOW));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkRestoreWindowPosition, constraints);
    constraints.gridy++;

    // Save window position
    chkSaveWindowPosition = Utilities.createJCheckBox(
        GT._("Save window position"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_SAVE_WINDOW,
            Configuration.DEFAULT_SAVE_WINDOW));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkSaveWindowPosition, constraints);
    constraints.gridy++;

    // Use short notation
    chkShortNotation = Utilities.createJCheckBox(
        GT._("Use short notation [[Xxxxx (yyy)|]]"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_SHORT_NOTATION,
            Configuration.DEFAULT_SHORT_NOTATION));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkShortNotation, constraints);
    constraints.gridy++;

    // Use advanced features
    chkAdvancedFeatures = Utilities.createJCheckBox(
        GT._("Use advanced features (experimental)"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ADVANCED_FEATURES,
            Configuration.DEFAULT_ADVANCED_FEATURES));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkAdvancedFeatures, constraints);
    constraints.gridy++;

    // Display "WikiCleaner" in the update comments
    chkWikiInComments = Utilities.createJCheckBox(
        GT._("Display WikiCleaner link in update comments"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_WIKICLEANER_COMMENT,
            Configuration.DEFAULT_WIKICLEANER_COMMENT));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkWikiInComments, constraints);
    constraints.gridy++;

    // Show 0 errors in Check Wiki
    chkShow0ErrorsCheckWiki = Utilities.createJCheckBox(
        GT._("Show errors with no detection found"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_CHECK_SHOW_0_ERRORS,
            Configuration.DEFAULT_CHECK_SHOW_0_ERRORS));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkShow0ErrorsCheckWiki, constraints);
    constraints.gridy++;

    // Link to error description in comment for Check Wiki
    chkLinkErrorsCheckWiki = Utilities.createJCheckBox(
        GT._("Add link to error description in comments"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_CHECK_LINK_ERRORS,
            Configuration.DEFAULT_CHECK_LINK_ERRORS));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkLinkErrorsCheckWiki, constraints);
    constraints.gridy++;

    // Force watching pages that have been edited
    chkForceWatch = Utilities.createJCheckBox(
        GT._("Watch all edited pages"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_FORCE_WATCH,
            Configuration.DEFAULT_FORCE_WATCH));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkForceWatch, constraints);
    constraints.gridy++;

    // Check orthograph
    chkOrthograph = Utilities.createJCheckBox(
        GT._("Check orthograph and typography"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_ORTHOGRAPH,
            Configuration.DEFAULT_ORTHOGRAPH));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chkOrthograph, constraints);
    constraints.gridy++;

    // Menu size
    modelMenuSize = new SpinnerNumberModel(
        configuration.getInt(
            null,
            Configuration.INTEGER_MENU_SIZE,
            Configuration.DEFAULT_MENU_SIZE),
        2, 999, 1);
    spinMenuSize = new JSpinner(modelMenuSize);
    JLabel labelMenuSize = Utilities.createJLabel(GT._("Maximum number of items in a menu :"));
    labelMenuSize.setLabelFor(spinMenuSize);
    labelMenuSize.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelMenuSize, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spinMenuSize, constraints);
    constraints.gridy++;

    // Max pages
    modelMaxPages = new SpinnerNumberModel(
        configuration.getInt(
            null,
            Configuration.INTEGER_MAXIMUM_PAGES,
            Configuration.DEFAULT_MAXIMUM_PAGES),
        1, 99, 1);
    spinMaxPages = new JSpinner(modelMaxPages);
    JLabel labelMaxPages = Utilities.createJLabel(GT._("Maximum number of simultaneous analysis :"));
    labelMaxPages.setLabelFor(spinMaxPages);
    labelMaxPages.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelMaxPages, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spinMaxPages, constraints);
    constraints.gridy++;

    // Max errors for Check Wiki
    modelMaxErrorsCheckWiki = new SpinnerNumberModel(
        configuration.getInt(
            null,
            Configuration.INTEGER_CHECK_NB_ERRORS,
            Configuration.DEFAULT_CHECK_NB_ERRORS),
        10, 1000, 5);
    spinMaxErrorsCheckWiki = new JSpinner(modelMaxErrorsCheckWiki);
    JLabel labelMaxErrorsCheckWiki = Utilities.createJLabel(
        GT._("Maximum number of errors for Check Wiki :"));
    labelMaxErrorsCheckWiki.setLabelFor(spinMaxErrorsCheckWiki);
    labelMaxErrorsCheckWiki.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelMaxErrorsCheckWiki, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spinMaxErrorsCheckWiki, constraints);
    constraints.gridy++;

    // Interrogation threads
    modelInterrogationThreads = new SpinnerNumberModel(
        configuration.getInt(
            null,
            Configuration.INTEGER_INTERROG_THREAD,
            Configuration.DEFAULT_INTERROG_THREAD),
        1, 99, 1);
    spinInterrogationThreads = new JSpinner(modelInterrogationThreads);
    JLabel labelThreads = Utilities.createJLabel(GT._("Maximum number of interrogation threads :"));
    labelThreads.setLabelFor(spinInterrogationThreads);
    labelThreads.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelThreads, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spinInterrogationThreads, constraints);
    constraints.gridy++;

    // Signature
    txtSignature = new JTextField(15);
    txtSignature.setText(configuration.getString(
        null,
        Configuration.STRING_SIGNATURE,
        Configuration.DEFAULT_SIGNATURE));
    JLabel labelSignature = Utilities.createJLabel(GT._("Signature :"));
    labelSignature.setLabelFor(txtSignature);
    labelSignature.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelSignature, constraints);
    constraints.gridwidth = 2;
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtSignature, constraints);
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
    add(emptyPanel, constraints);
  }

  /**
   * Restore all options to their default values.
   */
  @Override
  public void defaultValues() {
    Configuration config = Configuration.getConfiguration();

    // Boolean values
    chkAdvancedFeatures.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ADVANCED_FEATURES,
        Configuration.DEFAULT_ADVANCED_FEATURES));
    chkCloseDisambig.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CLOSE_DISAMBIG,
        Configuration.DEFAULT_CLOSE_DISAMBIG));
    chkForceWatch.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_FORCE_WATCH,
        Configuration.DEFAULT_FORCE_WATCH));
    chkLinkErrorsCheckWiki.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CHECK_LINK_ERRORS,
        Configuration.DEFAULT_CHECK_LINK_ERRORS));
    chkOrthograph.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_ORTHOGRAPH,
        Configuration.DEFAULT_ORTHOGRAPH));
    chkRestoreWindowPosition.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_RESTORE_WINDOW,
        Configuration.DEFAULT_RESTORE_WINDOW));
    chkSaveWindowPosition.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_SAVE_WINDOW,
        Configuration.DEFAULT_SAVE_WINDOW));
    chkShortNotation.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_SHORT_NOTATION,
        Configuration.DEFAULT_SHORT_NOTATION));
    chkShow0ErrorsCheckWiki.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_CHECK_SHOW_0_ERRORS,
        Configuration.DEFAULT_CHECK_SHOW_0_ERRORS));
    chkWikiInComments.setSelected(config.getBoolean(
        null,
        Configuration.BOOLEAN_WIKICLEANER_COMMENT,
        Configuration.DEFAULT_WIKICLEANER_COMMENT));

    // Integer values
    modelInterrogationThreads.setValue(config.getInt(
        null,
        Configuration.INTEGER_INTERROG_THREAD,
        Configuration.DEFAULT_INTERROG_THREAD));
    modelMaxErrorsCheckWiki.setValue(config.getInt(
        null,
        Configuration.INTEGER_CHECK_NB_ERRORS,
        Configuration.DEFAULT_CHECK_NB_ERRORS));
    modelMaxPages.setValue(config.getInt(
        null,
        Configuration.INTEGER_MAXIMUM_PAGES,
        Configuration.DEFAULT_MAXIMUM_PAGES));
    modelMenuSize.setValue(config.getInt(
        null,
        Configuration.INTEGER_MENU_SIZE,
        Configuration.DEFAULT_MENU_SIZE));

    // String values
    txtSignature.setText(config.getString(
        null,
        Configuration.STRING_SIGNATURE,
        Configuration.DEFAULT_SIGNATURE));
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
        Configuration.BOOLEAN_ADVANCED_FEATURES,
        chkAdvancedFeatures.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CLOSE_DISAMBIG,
        chkCloseDisambig.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_FORCE_WATCH,
        chkForceWatch.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CHECK_LINK_ERRORS,
        chkLinkErrorsCheckWiki.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_ORTHOGRAPH,
        chkOrthograph.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_RESTORE_WINDOW,
        chkRestoreWindowPosition.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_SAVE_WINDOW,
        chkSaveWindowPosition.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_SHORT_NOTATION,
        chkShortNotation.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_CHECK_SHOW_0_ERRORS,
        chkShow0ErrorsCheckWiki.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_WIKICLEANER_COMMENT,
        chkWikiInComments.isSelected());

    // Integer values
    config.setInt(
        null,
        Configuration.INTEGER_INTERROG_THREAD,
        modelInterrogationThreads.getNumber().intValue());
    config.setInt(
        null,
        Configuration.INTEGER_CHECK_NB_ERRORS,
        modelMaxErrorsCheckWiki.getNumber().intValue());
    config.setInt(
        null,
        Configuration.INTEGER_MAXIMUM_PAGES,
        modelMaxPages.getNumber().intValue());
    config.setInt(
        null,
        Configuration.INTEGER_MENU_SIZE,
        modelMenuSize.getNumber().intValue());

    // String values
    config.setString(
        null,
        Configuration.STRING_SIGNATURE,
        txtSignature.getText());
  }
}
