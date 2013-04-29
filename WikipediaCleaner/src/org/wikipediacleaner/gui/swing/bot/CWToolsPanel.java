/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.bot;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


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
   * Configure if page that couldn't be fixed should be analyzed.
   */
  private JCheckBox chkCWAnalyze;

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

    // Label
    JLabel labelCW = Utilities.createJLabel(GT._("Automatic fixing for Check Wiki"));
    add(labelCW, constraints);
    constraints.gridy++;

    // Table for listing errors
    modelCWAutomaticFixing = new BotCWTableModel(window.getWikipedia());
    tableCWAutomaticFixing = new JTable(modelCWAutomaticFixing);
    modelCWAutomaticFixing.configureColumnModel(tableCWAutomaticFixing.getColumnModel());
    Utilities.addRowSorter(tableCWAutomaticFixing, modelCWAutomaticFixing);
    JScrollPane paneCWAutomaticFixing = new JScrollPane(tableCWAutomaticFixing);
    paneCWAutomaticFixing.setMinimumSize(new Dimension(200, 200));
    paneCWAutomaticFixing.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
    paneCWAutomaticFixing.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.weighty = 1;
    add(paneCWAutomaticFixing, constraints);
    constraints.gridy++;
    constraints.weighty = 0;

    // Button for running the automatic fixing
    buttonCWAutomaticFixing = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Automatic fixing for Check Wiki"), false);
    buttonCWAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCWAutomaticFixing"));
    add(buttonCWAutomaticFixing, constraints);
    constraints.gridy++;

    // Check box to decide if pages should be analyzed if not fixed
    chkCWAnalyze = Utilities.createJCheckBox(
        GT._("Analyze pages that couldn't be fixed by bot"), true);
    add(chkCWAnalyze, constraints);
    constraints.gridy++;

    // Select errors that can be fixed by bot
    ListSelectionModel selectionModel = tableCWAutomaticFixing.getSelectionModel();
    selectionModel.clearSelection();
    for (int i = 0; i < modelCWAutomaticFixing.getRowCount(); i++) {
      if (modelCWAutomaticFixing.isBotAlgorithm(i)) {
        selectionModel.addSelectionInterval(i, i);
      }
    }
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
    EnumWikipedia wiki = window.getWikipedia();
    if (!wiki.getCWConfiguration().isProjectAvailable()) {
      Utilities.displayMissingConfiguration(
          window.getParentComponent(), null);
      return;
    }
    int[] selection = tableCWAutomaticFixing.getSelectedRows();
    if ((selection == null) || (selection.length == 0)) {
      return;
    }
    List<CheckErrorAlgorithm> selectedAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    for (int i = 0; i < selection.length; i++) {
      selectedAlgorithms.add(modelCWAutomaticFixing.getAlgorithm(selection[i]));
    }
    int answer = window.displayYesNoWarning(BasicWindow.experimentalMessage);
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    int max = 100;
    String maxString = window.askForValue(
        GT._("How many pages do you want to analyze"),
        Integer.toString(max), null);
    if (maxString == null) {
      return;
    }
    try {
      max = Integer.parseInt(maxString);
    } catch (NumberFormatException e) {
      return;
    }
    AutomaticCWWorker worker = new AutomaticCWWorker(
        wiki, window,
        selectedAlgorithms, max,
        modelCWAutomaticFixing.getAlgorithms(),
        chkCWAnalyze.isSelected());
    worker.start();
  }
}
