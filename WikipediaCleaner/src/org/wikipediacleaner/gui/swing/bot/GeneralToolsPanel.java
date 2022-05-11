/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateDabWarningWorker;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateDuplicateArgsWarningWorker;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateISBNWarningWorker;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateISSNWarningWorker;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateUnknownParameterWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A panel for general bot tools.
 */
public class GeneralToolsPanel extends BotToolsPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 211233278326264003L;

  /**
   * Button for automatic fixing.
   */
  private JButton buttonAutomaticFixing;

  /**
   * Button for monitoring recent changes.
   */
  private JButton buttonMonitorRC;

  /**
   * Button for updating disambiguation warnings on all wiki.
   */
  private JButton buttonUpdateDabWarning;

  /**
   * Button for updating ISBN warnings on all wiki.
   */
  private JButton buttonUpdateISBNWarning;

  /**
   * Button for updating ISSN warnings on all wiki.
   */
  private JButton buttonUpdateISSNWarning;

  /**
   * Button for updating duplicate arguments warnings on all wiki.
   */
  private JButton buttonUpdateDuplicateArgsWarning;

  /**
   * Button for updating unknown parameter warnings on all wiki.
   */
  private JButton buttonUpdateUnknownParameterWarning;

  /**
   * Button for listing ISBN warnings on all wiki.
   */
  private JButton buttonListISBNError;

  /**
   * Button for listing ISSN warnings on all wiki.
   */
  private JButton buttonListISSNError;

  /**
   * Construct a general bot tools panel.
   * 
   * @param window Parent window.
   */
  public GeneralToolsPanel(BasicWindow window) {
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

    // Automatic disambiguation fixing
    buttonAutomaticFixing = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._T("Semi-automatic disambiguation fixing"), true, null);
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAutomaticFixing"));
    add(buttonAutomaticFixing, constraints);
    constraints.gridy++;

    // Update disambiguation warning
    buttonUpdateDabWarning = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._T("Update existing disambiguation warning messages"), true, null);
    buttonUpdateDabWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateDabWarning"));
    add(buttonUpdateDabWarning, constraints);
    constraints.gridy++;

    // Update ISBN warning
    buttonUpdateISBNWarning = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._T("Update {0} warning messages", "ISBN"), true, null);
    buttonUpdateISBNWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateISBNWarning"));
    add(buttonUpdateISBNWarning, constraints);
    constraints.gridy++;

    // List ISBN errors
    buttonListISBNError = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._T("List {0} errors", "ISBN"), true, null);
    buttonListISBNError.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionListISBNErrors"));
    add(buttonListISBNError, constraints);
    constraints.gridy++;

    // Update ISSN warning
    buttonUpdateISSNWarning = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._T("Update {0} warning messages", "ISSN"), true, null);
    buttonUpdateISSNWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateISSNWarning"));
    add(buttonUpdateISSNWarning, constraints);
    constraints.gridy++;

    // List ISSN errors
    buttonListISSNError = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._T("List {0} errors", "ISSN"), true, null);
    buttonListISSNError.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionListISSNErrors"));
    add(buttonListISSNError, constraints);
    constraints.gridy++;

    // Update duplicate arguments warning
    buttonUpdateDuplicateArgsWarning = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._T("Update duplicate argument warning messages"), true, null);
    buttonUpdateDuplicateArgsWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateDuplicateArgsWarning"));
    add(buttonUpdateDuplicateArgsWarning, constraints);
    constraints.gridy++;

    // Update unknown parameter warning
    buttonUpdateUnknownParameterWarning = Utilities.createJButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._T("Update unknown parameter warning messages"), true, null);
    buttonUpdateUnknownParameterWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdateUnknownParameterWarning"));
    add(buttonUpdateUnknownParameterWarning, constraints);
    constraints.gridy++;

    // Monitor recent changes
    buttonMonitorRC = Utilities.createJButton(
        "commons-nuvola-apps-kcmsystem.png", EnumImageSize.NORMAL,
        GT._T("Monitor recent changes"), true, null);
    buttonMonitorRC.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMonitorRC"));
    add(buttonMonitorRC, constraints);
    constraints.gridy++;

    // Empty panel
    JPanel panel = new JPanel();
    constraints.weighty = 1;
    add(panel, constraints);
    constraints.gridy++;
  }

  /**
   * Update components state.
   */
  @Override
  protected void updateComponentState() {
    buttonAutomaticFixing.setEnabled(false);
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
   * Action called when Update Disambiguation Warning button is pressed.
   */
  public void actionUpdateDabWarning() {
    EnumWikipedia wiki = window.getWikipedia();
    Configuration config = Configuration.getConfiguration();
    WPCConfiguration wpcConfig = wiki.getConfiguration();
    String template = wpcConfig.getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          window.getParentComponent(),
          WPCConfigurationString.DAB_WARNING_TEMPLATE.getAttributeName());
      return;
    }
    String start = window.askForValue(
        GT._T("At what page do you wish to start updating the warnings ?"),
        config.getString(null, ConfigurationValueString.LAST_DAB_WARNING), null);
    if (start == null) {
      return;
    }
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(
        wiki, window, start);
    worker.start();
  }

  /**
   * Action called when Update ISBN Warning button is pressed.
   */
  public void actionUpdateISBNWarning() {
    actionISBNWarning(false);
  }

  /**
   * Action called when List ISBN Errors button is pressed.
   */
  public void actionListISBNErrors() {
    actionISBNWarning(true);
  }

  /**
   * Analyze ISBN errors.
   * 
   * @param simulation True if this is a simulation.
   */
  private void actionISBNWarning(boolean simulation) {
    EnumWikipedia wiki = window.getWikipedia();
    if (!simulation) {
      WPCConfiguration wpcConfig = wiki.getConfiguration();
      String template = wpcConfig.getString(WPCConfigurationString.ISBN_WARNING_TEMPLATE);
      if ((template == null) || (template.trim().length() == 0)) {
        Utilities.displayMessageForMissingConfiguration(
            window.getParentComponent(),
            WPCConfigurationString.ISBN_WARNING_TEMPLATE.getAttributeName());
        return;
      }
    }
    UpdateISBNWarningWorker worker = new UpdateISBNWarningWorker(
        wiki, window, simulation);
    worker.start();
  }

  /**
   * Action called when Update ISSN Warning button is pressed.
   */
  public void actionUpdateISSNWarning() {
    actionISSNWarning(false);
  }

  /**
   * Action called when List ISSN Errors button is pressed.
   */
  public void actionListISSNErrors() {
    actionISSNWarning(true);
  }

  /**
   * Analyze ISSN errors.
   * 
   * @param simulation True if this is a simulation.
   */
  private void actionISSNWarning(boolean simulation) {
    EnumWikipedia wiki = window.getWikipedia();
    if (!simulation) {
      WPCConfiguration wpcConfig = wiki.getConfiguration();
      String template = wpcConfig.getString(WPCConfigurationString.ISSN_WARNING_TEMPLATE);
      if ((template == null) || (template.trim().length() == 0)) {
        Utilities.displayMessageForMissingConfiguration(
            window.getParentComponent(),
            WPCConfigurationString.ISSN_WARNING_TEMPLATE.getAttributeName());
        return;
      }
    }
    UpdateISSNWarningWorker worker = new UpdateISSNWarningWorker(
        wiki, window, simulation);
    worker.start();
  }

  /**
   * Action called when Update Duplicate Arguments Warning button is pressed.
   */
  public void actionUpdateDuplicateArgsWarning() {
    EnumWikipedia wiki = window.getWikipedia();
    WPCConfiguration wpcConfig = wiki.getConfiguration();
    String template = wpcConfig.getString(WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          window.getParentComponent(),
          WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE.getAttributeName());
      return;
    }
    UpdateDuplicateArgsWarningWorker worker = new UpdateDuplicateArgsWarningWorker(
        wiki, window, false);
    worker.start();
  }

  /**
   * Action called when Update Unknown Parameter Warning button is pressed.
   */
  public void actionUpdateUnknownParameterWarning() {
    EnumWikipedia wiki = window.getWikipedia();
    WPCConfiguration wpcConfig = wiki.getConfiguration();
    String template = wpcConfig.getString(WPCConfigurationString.UNKNOWN_PARAMETER_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          window.getParentComponent(),
          WPCConfigurationString.UNKNOWN_PARAMETER_WARNING_TEMPLATE.getAttributeName());
      return;
    }
    UpdateUnknownParameterWarningWorker worker = new UpdateUnknownParameterWarningWorker(
        wiki, window, false);
    worker.start();
  }

  /**
   * Action called when Monitor Recent Changes button is pressed.
   */
  public void actionMonitorRC() {
    int answer = window.displayYesNoWarning(BasicWindow.experimentalMessage);
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    Controller.runMonitorRC(window.getWikipedia());
  }
}
