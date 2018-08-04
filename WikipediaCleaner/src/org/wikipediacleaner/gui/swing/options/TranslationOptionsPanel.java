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

import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * A panel for translation options.
 */
public class TranslationOptionsPanel extends OptionsPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = -5652387893294154836L;

  /**
   * Construct a Translation Options panel. 
   */
  public TranslationOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._T("Translation options")));
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

    // Internal links text
    chk = createJCheckBox(
        GT._T("Translate internal links text"),
        ConfigurationValueBoolean.TRANSLATION_INTERNAL_LINK_TEXT);
    add(chk, constraints);
    constraints.gridy++;

    // Inter language links
    chk = createJCheckBox(
        GT._T("Convert internal links without interwiki to inter language links"),
        ConfigurationValueBoolean.TRANSLATION_INTERLANGUAGE);
    add(chk, constraints);
    constraints.gridy++;

    // Categories
    chk = createJCheckBox(
        GT._T("Translate categories"),
        ConfigurationValueBoolean.TRANSLATION_CATEGORY);
    add(chk, constraints);
    constraints.gridy++;

    // Templates names
    chk = createJCheckBox(
        GT._T("Translate templates names"),
        ConfigurationValueBoolean.TRANSLATION_TEMPLATE_NAME);
    add(chk, constraints);
    constraints.gridy++;

    // Templates without parameters
    chk = createJCheckBox(
        GT._T("Translate templates without parameters"),
        ConfigurationValueBoolean.TRANSLATION_TEMPLATE_NO_PARAM);
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
