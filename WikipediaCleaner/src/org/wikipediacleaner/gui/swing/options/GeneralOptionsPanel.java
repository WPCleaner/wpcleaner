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
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A panel for general options. 
 */
public class GeneralOptionsPanel extends OptionsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -7125394212316622303L;

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
        BorderFactory.createEtchedBorder(), GT._T("General options")));
    JCheckBox chk = null;
    JTextField txt = null;

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

    // Use HTTP for API instead of HTTPS
    chk = createJCheckBox(
        GT._T("Use HTTP for MediaWiki API calls (less secure)"),
        ConfigurationValueBoolean.FORCE_HTTP_API);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Use secure URL in external viewer
    chk = createJCheckBox(
        GT._T("Use secure URL in external viewer (https)"),
        ConfigurationValueBoolean.SECURE_URL);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Mark edits as minor
    chk = createJCheckBox(
        GT._T("Mark edits as minor"),
        ConfigurationValueBoolean.MARK_EDIT_MINOR);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Close disambiguation window after sending
    chk = createJCheckBox(
        GT._T("Close disambiguation window after sending"),
        ConfigurationValueBoolean.CLOSE_DISAMBIG);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Add a note on talk page when requesting help
    chk = createJCheckBox(
        GT._T("Add a note on talk page when requesting help"),
        ConfigurationValueBoolean.ADD_NOTE_FOR_HELP);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Ignore disambiguation links from User NS
    chk = createJCheckBox(
        GT._T("Ignore disambiguation links from User namespace"),
        ConfigurationValueBoolean.IGNORE_DAB_USER_NS);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Restore window position
    chk = createJCheckBox(
        GT._T("Restore window position"),
        ConfigurationValueBoolean.RESTORE_WINDOW);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Save window position
    chk = createJCheckBox(
        GT._T("Save window position"),
        ConfigurationValueBoolean.SAVE_WINDOW);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Use short notation
    chk = createJCheckBox(
        GT._T("Use short notation [[Xxxxx (yyy)|]]"),
        ConfigurationValueBoolean.SHORT_NOTATION);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Use advanced features
    chk = createJCheckBox(
        GT._T("Use advanced features (experimental)"),
        ConfigurationValueBoolean.ADVANCED_FEATURES);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Display "WPCleaner" in the update comments
    chk = createJCheckBox(
        GT._T("Display {0} link in update comments", Version.PROGRAM),
        ConfigurationValueBoolean.WIKICLEANER_COMMENT);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Show 0 errors in Check Wiki
    chk = createJCheckBox(
        GT._T("Show errors with no detection found"),
        ConfigurationValueBoolean.CHECK_SHOW_0_ERRORS);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Link to error description in comment for Check Wiki
    chk = createJCheckBox(
        GT._T("Add link to error description in comments"),
        ConfigurationValueBoolean.CHECK_LINK_ERRORS);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Force watching pages that have been edited
    chk = createJCheckBox(
        GT._T("Watch all edited pages"),
        ConfigurationValueBoolean.FORCE_WATCH);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Check spelling
    chk = createJCheckBox(
        GT._T("Check spelling and typography"),
        ConfigurationValueBoolean.SPELLING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Copy/paste option
    chk = createJCheckBox(
        GT._T("Copy/paste in popup menu"),
        ConfigurationValueBoolean.COPY_PASTE);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(chk, constraints);
    constraints.gridy++;

    // Signature
    txt = createJTextField(ConfigurationValueString.SIGNATURE, 15);
    JLabel labelSignature = Utilities.createJLabel(GT._T("Signature:"));
    labelSignature.setLabelFor(txt);
    labelSignature.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelSignature, constraints);
    constraints.gridwidth = 2;
    constraints.gridx++;
    constraints.weightx = 1;
    add(txt, constraints);
    constraints.gridy++;

    // Comment
    txt = createJTextField(ConfigurationValueString.COMMENT, 15);
    JLabel labelComment = Utilities.createJLabel(GT._T("Comment:"));
    labelComment.setLabelFor(txt);
    labelComment.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelComment, constraints);
    constraints.gridwidth = 2;
    constraints.gridx++;
    constraints.weightx = 1;
    add(txt, constraints);
    constraints.gridy++;

    // Look and Feel
    JPanel plafPanel = new JPanel();
    plafPanel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("Look & Feel")));
    plafPanel.setLayout(new BoxLayout(plafPanel, BoxLayout.PAGE_AXIS));
    ButtonGroup plafGroup = new ButtonGroup();
    JRadioButton radPlafWPCleaner = Utilities.createJRadioButton(
        GT._T("Let {0} choose the Look && Feel", Version.PROGRAM),
        false);
    plafPanel.add(radPlafWPCleaner);
    plafGroup.add(radPlafWPCleaner);
    JRadioButton radPlafSystem = Utilities.createJRadioButton(
        GT._T("Use System Look && Feel"),
        false);
    plafPanel.add(radPlafSystem);
    plafGroup.add(radPlafSystem);
    JRadioButton radPlafUser = Utilities.createJRadioButton(
        GT._T("Choose Look && Feel"),
        false);
    plafPanel.add(radPlafUser);
    plafGroup.add(radPlafUser);
    setButtonGroup(ConfigurationValueInteger.PLAF_TYPE, plafGroup);
    Vector<String> plaf = new Vector<String>();
    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
      plaf.add(info.getName());
    }
    plafPanel.add(createJComboBox(ConfigurationValueString.PLAF_NAME, plaf));
    constraints.gridwidth = 3;
    constraints.gridx = 0;
    constraints.weightx = 1;
    add(plafPanel, constraints);
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
}
