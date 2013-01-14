/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.AutomaticCWWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A window for bot tools.
 */
public class BotToolsWindow
  extends BasicWindow {

  public final static Integer WINDOW_VERSION = Integer.valueOf(3);

  private JButton buttonAutomaticFixing;
  private JButton buttonCWAutomaticFixing;
  private JButton buttonMonitorRC;
  private JButton buttonUpdateDabWarning;

  private JCheckBox chkCWAnalyze;

  private JList lstCWAutomaticFixing;

  private Vector<CheckErrorAlgorithm> algorithms;

  /**
   * Create and display a BotToolsWindow.
   * 
   * @param wikipedia Wikipedia.
   */
  public static void createBotToolsWindow(
      final EnumWikipedia wikipedia) {
    createWindow(
        "BotToolsWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        BotToolsWindow.class,
        null);
  }

  /**
   * @return Window title.
   */
  @Override
  public String getTitle() {
    return GT._("Bot tools");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Warning
    String txtWarning =
      GT._("!!! WARNING !!!") + "\n" +
      GT._("Functions available here are considered as bot tools.") + "\n" +
      GT._("They may modify a lot of pages in a short period of time.") + "\n" +
      GT._("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
      GT._("Please, check if you need the bot status by reading the rules of Wikipedia.");
    JTextArea lblWarning = new JTextArea(txtWarning);
    lblWarning.setEditable(false);
    lblWarning.setBackground(getParentComponent().getBackground());
    lblWarning.setForeground(Color.RED);
    constraints.gridx = 0;
    constraints.weighty = 1;
    panel.add(lblWarning, constraints);
    constraints.gridy++;
    constraints.weighty = 0;

    // Tools : automatic disambiguation fixing
    buttonAutomaticFixing = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Semi-automatic disambiguation fixing"), true);
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAutomaticFixing"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    panel.add(buttonAutomaticFixing, constraints);
    constraints.gridy++;

    // Tools : update disambiguation warning
    buttonUpdateDabWarning = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Update existing disambiguation warning messages"), true);
    buttonUpdateDabWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateDabWarning"));
    panel.add(buttonUpdateDabWarning, constraints);
    constraints.gridy++;

    // Tools : monitor recent changes
    buttonMonitorRC = Utilities.createJButton(
        "commons-nuvola-apps-kcmsystem.png", EnumImageSize.NORMAL,
        GT._("Monitor recent changes"), true);
    buttonMonitorRC.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMonitorRC"));
    panel.add(buttonMonitorRC, constraints);
    constraints.gridy++;

    // Tools : automatic Check Wiki fixing
    algorithms = new Vector<CheckErrorAlgorithm>();
    addAlgorithm(2);  // Article with false <br/>
    addAlgorithm(6);  // DEFAULTSORT with special letter
    addAlgorithm(7);  // Headlines all start with three "="
    addAlgorithm(9);  // Categories more at one line
    addAlgorithm(17); // Category duplication
    addAlgorithm(19); // Headlines start with one "="
    addAlgorithm(18); // Category first letter small
    addAlgorithm(22); // Category with space
    addAlgorithm(25); // Headline hierarchy
    addAlgorithm(45); // Interwiki double
    addAlgorithm(54); // Break in list
    addAlgorithm(57); // Headlines end with colon
    addAlgorithm(64); // Link equal to link text
    addAlgorithm(88); // DEFAULTSORT with blank at first position
    addAlgorithm(92); // Headline double
    JPanel panelCW = new JPanel(new GridBagLayout());
    panelCW.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(),
        GT._("Automatic fixing for Check Wiki")));
    GridBagConstraints constraintsCW = new GridBagConstraints();
    constraintsCW.fill = GridBagConstraints.BOTH;
    constraintsCW.gridheight = 1;
    constraintsCW.gridwidth = 1;
    constraintsCW.gridx = 0;
    constraintsCW.gridy = 0;
    constraintsCW.insets = new Insets(0, 0, 0, 0);
    constraintsCW.ipadx = 0;
    constraintsCW.ipady = 0;
    constraintsCW.weightx = 1;
    constraintsCW.weighty = 1;
    lstCWAutomaticFixing = new JList(algorithms);
    lstCWAutomaticFixing.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    JScrollPane paneCWAutomaticFixing = new JScrollPane(lstCWAutomaticFixing);
    paneCWAutomaticFixing.setMinimumSize(new Dimension(100, 100));
    paneCWAutomaticFixing.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
    paneCWAutomaticFixing.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panelCW.add(paneCWAutomaticFixing, constraintsCW);
    constraintsCW.gridy++;
    buttonCWAutomaticFixing = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Automatic fixing for Check Wiki"), false);
    buttonCWAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCWAutomaticFixing"));
    constraintsCW.weighty = 0;
    panelCW.add(buttonCWAutomaticFixing, constraintsCW);
    constraintsCW.gridy++;
    chkCWAnalyze = Utilities.createJCheckBox(
        GT._("Analyze pages that couldn't be fixed by bot"), true);
    panelCW.add(chkCWAnalyze, constraintsCW);
    constraintsCW.gridy++;
    // TODO: Add check box for analyzing pages that couldn't be fixed
    panel.add(panelCW, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
    buttonPanel.add(buttonClose);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * Add an algorithm to the list of algorithms that can be fixed automatically.
   * 
   * @param errorNumber Error number.
   */
  private void addAlgorithm(int errorNumber) {
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(getWikipedia(), errorNumber);
    if ((algorithm != null) &&
        (algorithm.isAvailable()) &&
        CheckErrorAlgorithms.isAlgorithmActive(getWikipedia(), errorNumber)) {
      algorithms.add(algorithm);
    }
  }

  /**
   * Update components states.
   */
  @Override
  protected void updateComponentState() {
    super.updateComponentState();
    buttonAutomaticFixing.setEnabled(false);
    buttonCWAutomaticFixing.setEnabled(true);
    buttonMonitorRC.setEnabled(true);
    buttonUpdateDabWarning.setEnabled(true);
  }

  /**
   * Action called when Automatic Fixing button is pressed.
   */
  public void actionAutomaticFixing() {
    // TODO
  }

  /**
   * Action called when Automatic Check Wiki Fixing button is pressed.
   */
  public void actionCWAutomaticFixing() {
    Object[] selection = lstCWAutomaticFixing.getSelectedValues();
    if ((selection == null) || (selection.length == 0)) {
      return;
    }
    List<CheckErrorAlgorithm> selectedAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    for (int i = 0; i < selection.length; i++) {
      if (selection[i] instanceof CheckErrorAlgorithm) {
        selectedAlgorithms.add((CheckErrorAlgorithm) selection[i]);
      }
    }
    if (displayYesNoWarning(experimentalMessage) != JOptionPane.YES_OPTION) {
      return;
    }
    int max = 100;
    String maxString = askForValue(
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
        getWikipedia(), this, selectedAlgorithms, max, algorithms, chkCWAnalyze.isSelected());
    worker.start();
  }

  /**
   * Action called when Monitor Recent Changes button is pressed.
   */
  public void actionMonitorRC() {
    if (displayYesNoWarning(experimentalMessage) != JOptionPane.YES_OPTION) {
      return;
    }
    Controller.runMonitorRC(getWikipedia());
  }

  /**
   * Action called when Update Disambiguation Warning button is pressed.
   */
  public void actionUpdateDabWarning() {
    Configuration config = Configuration.getConfiguration();
    String start = askForValue(
        GT._("At what page do you wish to start updating the disambiguation warning ?"),
        config.getString(null, ConfigurationValueString.LAST_DAB_WARNING), null);
    if (start == null) {
      return;
    }
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(
        getWikipedia(), this, start);
    worker.start();
  }
}
