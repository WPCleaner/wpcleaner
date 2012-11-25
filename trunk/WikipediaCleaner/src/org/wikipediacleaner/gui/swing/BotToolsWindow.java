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
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.ToolServer;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
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

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  private JButton buttonAutomaticFixing;
  private JButton buttonCWAutomaticFixing;
  private JButton buttonMonitorRC;
  private JButton buttonUpdateDabWarning;

  private JComboBox cmbCWAutomaticFixing;

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
    constraints.weightx = 0;
    panel.add(lblWarning, constraints);
    constraints.gridy++;

    // Tools : automatic disambiguation fixing
    buttonAutomaticFixing = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Semi-automatic disambiguation fixing"), true);
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAutomaticFixing"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 0;
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
    addAlgorithm(18); // Category first letter small
    addAlgorithm(22); // Category with space
    addAlgorithm(64); // Link equal to link text
    addAlgorithm(88); // DEFAULTSORT with blank at first position
    cmbCWAutomaticFixing = new JComboBox(algorithms);
    panel.add(cmbCWAutomaticFixing, constraints);
    constraints.gridy++;
    buttonCWAutomaticFixing = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Automatic fixing for Check Wiki"), true);
    buttonCWAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCWAutomaticFixing"));
    panel.add(buttonCWAutomaticFixing, constraints);
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
    buttonCWAutomaticFixing.setEnabled(cmbCWAutomaticFixing.getItemCount() > 0);
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
    Object selection = cmbCWAutomaticFixing.getSelectedItem();
    if ((selection == null) || !(selection instanceof CheckErrorAlgorithm)) {
      return;
    }
    String message = "This function is experimental. Use at your own risk.\nDo you want to proceed ?";
    if (displayYesNoWarning(message) != JOptionPane.YES_OPTION) {
      return;
    }
    CheckErrorAlgorithm algorithm = (CheckErrorAlgorithm) selection;
    List<CheckError> errors = new ArrayList<CheckError>();
    int count = 0;
    try {
      API api = APIFactory.getAPI();
      ToolServer toolServer = APIFactory.getToolServer();
      toolServer.retrievePagesForError(algorithm, 100, getWikipedia(), errors);
      for (CheckError error : errors) {
        for (int numPage = 0;
            (numPage < error.getPageCount()) && (getParentComponent().isDisplayable());
            numPage++) {
          Page page = error.getPage(numPage);
          api.retrieveContents(getWikipedia(), Collections.singletonList(page), false);
          PageAnalysis analysis = page.getAnalysis(page.getContents(), true);
          List<CheckErrorPage> errorPages = CheckError.analyzeErrors(algorithms, analysis);
          boolean found = false;
          if (errorPages != null) {
            for (CheckErrorPage errorPage : errorPages) {
              if (algorithm.equals(errorPage.getAlgorithm()) &&
                  errorPage.getErrorFound()) {
                found = true;
              }
            }
          }
          if (found) {
            String newContents = page.getContents();
            List<CheckErrorAlgorithm> usedAlgorithms = new ArrayList<CheckErrorAlgorithm>();
            for (CheckErrorAlgorithm currentAlgorithm : algorithms) {
              String tmpContents = newContents;
              analysis = page.getAnalysis(tmpContents, true);
              newContents = currentAlgorithm.botFix(analysis);
              if (!newContents.equals(tmpContents)) {
                usedAlgorithms.add(currentAlgorithm);
              }
            }
            if (!newContents.equals(page.getContents())) {
              StringBuilder comment = new StringBuilder();
              comment.append(getWikipedia().getCWConfiguration().getComment());
              for (CheckErrorAlgorithm usedAlgorithm : usedAlgorithms) {
                comment.append(" - ");
                comment.append(usedAlgorithm.getShortDescriptionReplaced());
              }
              api.updatePage(
                  getWikipedia(), page, newContents,
                  getWikipedia().createUpdatePageComment(comment.toString(), null),
                  false);
              count++;
              for (CheckErrorAlgorithm usedAlgorithm : usedAlgorithms) {
                toolServer.markPageAsFixed(page, usedAlgorithm.getErrorNumberString());
              }
            }
          }
        }
      }
    } catch (APIException e) {
      getLog().error("Error fixing Check Wiki errors", e);
    }
    displayInformationMessage(GT._("{0} pages have been fixed", Integer.toString(count)));
  }

  /**
   * Action called when Monitor Recent Changes button is pressed.
   */
  public void actionMonitorRC() {
    String message = "This function is experimental. Use at your own risk.\nDo you want to proceed ?";
    if (displayYesNoWarning(message) != JOptionPane.YES_OPTION) {
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
