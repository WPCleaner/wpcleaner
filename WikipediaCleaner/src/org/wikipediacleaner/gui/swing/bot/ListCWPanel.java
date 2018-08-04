/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.bot;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.beans.EventHandler;
import java.io.File;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A panel for configuring the List Check Wiki tool.
 */
public class ListCWPanel extends JPanel {

  /** Serialization */
  private static final long serialVersionUID = -2902740097721237193L;

  /** Wiki */
  private final EnumWikipedia wiki;

  /** Text field for the dump file */
  private JTextField txtDumpFile;

  /** Radio button to choose to export to a directory */
  private JRadioButton radExportDir;

  /** Text field for the export directory */
  private JTextField txtExportDir;

  /** Button to choose in which directory to export */
  private JButton buttonExportDir;

  /** Radio button to choose to export to a page */
  private JRadioButton radExportPage;

  /** Text field for the export page */
  private JTextField txtExportPage;

  /** Check box to decide if last version of page should be checked before reporting an error */
  private JCheckBox chkCheckWiki;

  /** Check box to decide to check only pages previously reported */
  private JCheckBox chkOnlyRecheck;

  /**
   * Create a panel for configuring the List Check Wiki tool.
   * 
   * @param wiki Wiki.
   */
  public ListCWPanel(EnumWikipedia wiki) {
    super(new GridBagLayout(), true);
    this.wiki = wiki;
    constructContents();
  }

  /**
   * Construct panel contents.
   */
  private void constructContents() {
    GridBagConstraints constraints = new GridBagConstraints(
        0, 0, 1, 1, 1, 0,
        GridBagConstraints.LINE_START, GridBagConstraints.BOTH,
        new Insets(0, 0, 0, 0), 0, 0);
    Configuration config = Configuration.getConfiguration();

    // Dump file
    String lastDumpFile = config.getString(wiki, ConfigurationValueString.LAST_DUMP_FILE);
    txtDumpFile = Utilities.createJTextField(lastDumpFile, 40);
    JLabel labelDumpFile = Utilities.createJLabel(GT._T("Dump file:"));
    labelDumpFile.setLabelFor(txtDumpFile);
    JButton buttonDumpFile = Utilities.createJButton(
        "gnome-logviewer.png", EnumImageSize.SMALL,
        GT._T("Dump file"), false, null);
    buttonDumpFile.addActionListener(
        EventHandler.create(ActionListener.class, this, "actionDumpFile"));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(labelDumpFile, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtDumpFile, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    add(buttonDumpFile, constraints);
    constraints.gridy++;

    // Type of export
    ButtonGroup groupExport = new ButtonGroup();
    boolean exportOnWiki = config.getBoolean(wiki, ConfigurationValueBoolean.DUMP_ON_WIKI);

    // Export directory
    String lastExportDir = config.getString(wiki, ConfigurationValueString.LAST_EXPORT_DIRECTORY);
    txtExportDir = Utilities.createJTextField(lastExportDir, 40);
    radExportDir = Utilities.createJRadioButton(GT._T("Export directory:"), !exportOnWiki);
    radExportDir.addItemListener(EventHandler.create(
        ItemListener.class, this, "updateComponentState"));
    groupExport.add(radExportDir);
    buttonExportDir = Utilities.createJButton(
        "gnome-folder.png", EnumImageSize.SMALL,
        GT._T("Export directory"), false, null);
    buttonExportDir.addActionListener(
        EventHandler.create(ActionListener.class, this, "actionExportDir"));
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(radExportDir, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtExportDir, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    add(buttonExportDir, constraints);
    constraints.gridy++;

    // Export page
    String lastExportPage = config.getString(wiki, ConfigurationValueString.LAST_EXPORT_PAGE);
    txtExportPage = Utilities.createJTextField(lastExportPage, 40);
    radExportPage = Utilities.createJRadioButton(GT._T("Export pages:"), exportOnWiki);
    radExportPage.addItemListener(EventHandler.create(
        ItemListener.class, this, "updateComponentState"));
    groupExport.add(radExportPage);
    constraints.gridx = 0;
    constraints.weightx = 0;
    add(radExportPage, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    add(txtExportPage, constraints);
    constraints.gridy++;

    // Check wiki
    boolean checkWiki = config.getBoolean(wiki, ConfigurationValueBoolean.DUMP_CHECK_WIKI);
    chkCheckWiki = Utilities.createJCheckBox(
        GT._T("Check last version of article before reporting an error"), checkWiki);
    constraints.gridx = 0;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    add(chkCheckWiki, constraints);
    constraints.gridy++;

    // Only check articles previously reported
    chkOnlyRecheck = Utilities.createJCheckBox(
        GT._T("Only check articles previously reported"), false);
    constraints.gridx = 0;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    add(chkOnlyRecheck, constraints);
    constraints.gridy++;

    updateComponentState();
  }

  /**
   * @return Path to the dump file.
   */
  public File getDumpFile() {
    String pathDumpFile = txtDumpFile.getText();
    if ((pathDumpFile == null) || (pathDumpFile.isEmpty())) {
      return null;
    }
    return new File(pathDumpFile);
  }

  /**
   * @return True if export is done on wiki.
   */
  public boolean exportOnWiki() {
    return radExportPage.isSelected();
  }

  /**
   * @return Path to the export directory.
   */
  public File getExportDir() {
    String pathExportDir = txtExportDir.getText();
    if ((pathExportDir == null) || (pathExportDir.isEmpty())) {
      return null;
    }
    return new File(pathExportDir);
  }

  /**
   * @return Export page name.
   */
  public String getExportPage() {
    String exportPage = txtExportPage.getText();
    if ((exportPage == null) || (exportPage.isEmpty())) {
      return null;
    }
    return exportPage;
  }

  /**
   * @return True if last version should be checked.
   */
  public boolean checkWiki() {
    return chkCheckWiki.isSelected();
  }

  /**
   * @return True to check only articles previously reported.
   */
  public boolean onlyRecheck() {
    return chkOnlyRecheck.isSelected();
  }

  /**
   * Save current configuration.
   */
  public void saveConfiguration() {
    Configuration config = Configuration.getConfiguration();
    config.setString(wiki, ConfigurationValueString.LAST_DUMP_FILE, txtDumpFile.getText());
    config.setBoolean(wiki, ConfigurationValueBoolean.DUMP_ON_WIKI, radExportPage.isSelected());
    config.setString(wiki, ConfigurationValueString.LAST_EXPORT_DIRECTORY, txtExportDir.getText());
    config.setString(wiki, ConfigurationValueString.LAST_EXPORT_PAGE, txtExportPage.getText());
    config.setBoolean(wiki, ConfigurationValueBoolean.DUMP_CHECK_WIKI, chkCheckWiki.isSelected());
  }

  /**
   * Action called when the Dump File button is clicked.
   */
  public void actionDumpFile() {
    JFileChooser fileChooser = new JFileChooser();
    File dumpFile = getDumpFile();
    fileChooser.setCurrentDirectory((dumpFile != null) ? dumpFile.getParentFile() : new File("."));
    fileChooser.setDialogTitle(GT._T("Dump file"));
    int answer = fileChooser.showOpenDialog(getParent());
    if (answer == JFileChooser.APPROVE_OPTION) {
      String pathDumpFile = fileChooser.getSelectedFile().getAbsolutePath();
      txtDumpFile.setText(pathDumpFile);
    }
  }

  /**
   * Action called when component state should be updated.
   */
  public void updateComponentState() {
    txtExportDir.setEnabled(radExportDir.isSelected());
    buttonExportDir.setEnabled(radExportDir.isSelected());
    txtExportPage.setEnabled(radExportPage.isSelected());
    chkOnlyRecheck.setEnabled(radExportPage.isSelected());
  }

  /**
   * Action called when the Export Directory button is clicked.
   */
  public void actionExportDir() {
    JFileChooser dirChooser = new JFileChooser();
    File exportDir = getExportDir();
    dirChooser.setCurrentDirectory((exportDir != null) ? exportDir : new File("."));
    dirChooser.setDialogTitle(GT._T("Export directory"));
    dirChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int answer = dirChooser.showOpenDialog(getParent());
    if (answer == JFileChooser.APPROVE_OPTION) {
      String pathExportDir = dirChooser.getSelectedFile().getAbsolutePath();
      txtExportDir.setText(pathExportDir);
    }
  }
}
