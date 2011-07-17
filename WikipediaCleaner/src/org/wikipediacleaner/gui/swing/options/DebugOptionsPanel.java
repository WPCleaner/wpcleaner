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

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A panel for debug options.
 */
public class DebugOptionsPanel extends OptionsPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = 3614825228324362078L;

  private JCheckBox chkDebugTime;
  private JCheckBox chkDebugURL;
  private JCheckBox chkDebugXML;

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
    constraints.weightx = 1;
    constraints.weighty = 0;

    constraints.gridwidth = 3;

    // Debug URL
    chkDebugURL = Utilities.createJCheckBox(
        GT._("Log all URL called by WikiCleaner"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_DEBUG_URL,
            Configuration.DEFAULT_DEBUG_URL));
    add(chkDebugURL, constraints);
    constraints.gridy++;

    // Debug XML
    chkDebugXML = Utilities.createJCheckBox(
        GT._("Log all answers to MediaWiki API calls"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_DEBUG_XML,
            Configuration.DEFAULT_DEBUG_XML));
    add(chkDebugXML, constraints);
    constraints.gridy++;

    // Debug time
    chkDebugTime = Utilities.createJCheckBox(
        GT._("Add a timestamp to logs"),
        configuration.getBoolean(
            null,
            Configuration.BOOLEAN_DEBUG_TIME,
            Configuration.DEFAULT_DEBUG_TIME));
    add(chkDebugTime, constraints);
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
    //
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
        Configuration.BOOLEAN_DEBUG_TIME,
        chkDebugTime.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_DEBUG_URL,
        chkDebugURL.isSelected());
    config.setBoolean(
        null,
        Configuration.BOOLEAN_DEBUG_XML,
        chkDebugXML.isSelected());
  }
}
