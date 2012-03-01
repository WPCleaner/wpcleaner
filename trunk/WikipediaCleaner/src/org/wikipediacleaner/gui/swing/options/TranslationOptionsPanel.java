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
        BorderFactory.createEtchedBorder(), GT._("Translation options")));
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
        GT._("Translate internal links text"),
        ConfigurationValueBoolean.TRANSLATION_INTERNAL_LINK_TEXT);
    add(chk, constraints);
    constraints.gridy++;

    // Inter language links
    chk = createJCheckBox(
        GT._("Convert internal links without interwiki to inter language links"),
        ConfigurationValueBoolean.TRANSLATION_INTERLANGUAGE);
    add(chk, constraints);
    constraints.gridy++;

    // Categories
    chk = createJCheckBox(
        GT._("Translate categories"),
        ConfigurationValueBoolean.TRANSLATION_CATEGORY);
    add(chk, constraints);
    constraints.gridy++;

    // Templates names
    chk = createJCheckBox(
        GT._("Translate templates names"),
        ConfigurationValueBoolean.TRANSLATION_TEMPLATE_NAME);
    add(chk, constraints);
    constraints.gridy++;

    // Templates without parameteres
    chk = createJCheckBox(
        GT._("Translate templates without parameters"),
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
