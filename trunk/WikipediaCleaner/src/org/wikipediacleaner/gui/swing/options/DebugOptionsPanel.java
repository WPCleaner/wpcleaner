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
import javax.swing.JPanel;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * A panel for debug options.
 */
public class DebugOptionsPanel extends OptionsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 3614825228324362078L;

  /**
   * Construct a Debug Options panel. 
   */
  public DebugOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Debug options")));
    JCheckBox chk = null;

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

    // Debug URL
    chk = createJCheckBox(
        GT._("Log all URLs called by {0}", Version.PROGRAM),
        ConfigurationValueBoolean.DEBUG_URL);
    add(chk, constraints);
    constraints.gridy++;

    // Debug API
    chk = createJCheckBox(
        GT._("Log all answers to MediaWiki API calls"),
        ConfigurationValueBoolean.DEBUG_API);
    add(chk, constraints);
    constraints.gridy++;

    // Debug time
    chk = createJCheckBox(
        GT._("Add a timestamp to logs"),
        ConfigurationValueBoolean.DEBUG_TIME);
    add(chk, constraints);
    constraints.gridy++;

    // Create log file
    chk = createJCheckBox(
        GT._("Use a log file"),
        ConfigurationValueBoolean.DEBUG_FILE);
    add(chk, constraints);
    constraints.gridy++;

    // Detailed debugging
    chk = createJCheckBox(
        GT._("Detailed debugging"),
        ConfigurationValueBoolean.DEBUG_DETAILS);
    add(chk, constraints);
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
