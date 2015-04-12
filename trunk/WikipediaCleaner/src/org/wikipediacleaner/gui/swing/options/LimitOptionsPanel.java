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
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SwingConstants;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * A panel for limits options. 
 */
public class LimitOptionsPanel extends OptionsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -7125394212316622303L;

  /**
   * Construct a Limits Options panel. 
   */
  public LimitOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Limits")));
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
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Menu size
    spin = createJSpinner(
        ConfigurationValueInteger.MENU_SIZE,
        2, 999, 1);
    JLabel labelMenuSize = Utilities.createJLabel(GT._("Maximum number of items in a menu :"));
    labelMenuSize.setLabelFor(spin);
    labelMenuSize.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelMenuSize, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Max pages
    spin = createJSpinner(
        ConfigurationValueInteger.MAXIMUM_PAGES,
        1, 99, 1);
    JLabel labelMaxPages = Utilities.createJLabel(GT._("Maximum number of simultaneous analyses :"));
    labelMaxPages.setLabelFor(spin);
    labelMaxPages.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelMaxPages, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Max errors for Check Wiki
    spin = createJSpinner(
        ConfigurationValueInteger.CHECK_NB_ERRORS,
        10, 1000, 5);
    JLabel labelMaxErrorsCheckWiki = Utilities.createJLabel(
        GT._("Maximum number of errors for Check Wiki :"));
    labelMaxErrorsCheckWiki.setLabelFor(spin);
    labelMaxErrorsCheckWiki.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelMaxErrorsCheckWiki, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Time between edits
    spin = createJSpinner(
        ConfigurationValueInteger.TIME_BETWEEN_EDIT,
        0, 120, 1);
    JLabel labelTime = Utilities.createJLabel(GT._("Minimum time between consecutive edits :"));
    labelTime.setLabelFor(spin);
    labelTime.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelTime, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Number of edits per minute
    spin = createJSpinner(
        ConfigurationValueInteger.MAX_EDITS_PER_MINUTE,
        0, 60, 1);
    JLabel labelEdits = Utilities.createJLabel(GT._("Maximum number of edits per minute :"));
    labelEdits.setLabelFor(spin);
    labelEdits.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelEdits, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Interrogation threads
    spin = createJSpinner(
        ConfigurationValueInteger.INTERROG_THREAD,
        1, 99, 1);
    JLabel labelThreads = Utilities.createJLabel(GT._("Maximum number of interrogation threads :"));
    labelThreads.setLabelFor(spin);
    labelThreads.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelThreads, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Add size limit for syntax highlighting
    spin = createJSpinner(
        ConfigurationValueInteger.SYNTAX_HIGHLIGHTING_LIMIT,
        0, 1000000, 10000);
    JLabel labelHighlight = Utilities.createJLabel(GT._("Size limit for syntax highlighting"));
    labelHighlight.setLabelFor(spin);
    labelHighlight.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelHighlight, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Add size limit for syntax highlighting
    spin = createJSpinner(
        ConfigurationValueInteger.SLOW_REGEXP,
        0, 1000000, 1000);
    JLabel labelRegexp = Utilities.createJLabel(GT._("Threshold for slow regular expressions (ms)"));
    labelRegexp.setLabelFor(spin);
    labelRegexp.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelRegexp, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Maximum number of category members
    spin = createJSpinner(
        ConfigurationValueInteger.MAX_CATEGORY_MEMBERS,
        100, 1000000, 1000);
    JLabel labelCategory = Utilities.createJLabel(GT._("Maximum number of category members :"));
    labelCategory.setLabelFor(spin);
    labelCategory.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelCategory, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Maximum number of days for abuse logs
    spin = createJSpinner(
        ConfigurationValueInteger.MAX_DAYS_ABUSE_LOG,
        1, 365, 1);
    JLabel labelAbuseLog = Utilities.createJLabel(GT._("Maximum duration for abuse logs :"));
    labelAbuseLog.setLabelFor(spin);
    labelAbuseLog.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelAbuseLog, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Maximum number of embedding pages
    spin = createJSpinner(
        ConfigurationValueInteger.MAX_EMBEDDED_IN,
        100, 1000000, 1000);
    JLabel labelEmbedded = Utilities.createJLabel(GT._("Maximum number of embedding pages :"));
    labelEmbedded.setLabelFor(spin);
    labelEmbedded.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelEmbedded, constraints);
    constraints.gridwidth = 1;
    constraints.gridx = 2;
    constraints.weightx = 1;
    add(spin, constraints);
    constraints.gridy++;

    // Maximum number of search results
    spin = createJSpinner(
        ConfigurationValueInteger.MAX_SEARCH,
        100, 1000000, 1000);
    JLabel labelSearch = Utilities.createJLabel(GT._("Maximum number of search results :"));
    labelSearch.setLabelFor(spin);
    labelSearch.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelSearch, constraints);
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
    constraints.gridwidth = 3;
    constraints.gridx = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weightx = 1;
    constraints.weighty = 1;
    add(emptyPanel, constraints);
  }
}
