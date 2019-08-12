/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.deadlink;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;


/**
 * A panel for a list of dead links.
 */
public class DeadLinkPanel extends JPanel {

  /** Serialization */
  private static final long serialVersionUID = -3583784531089028512L;

  /** Wiki */
  private final EnumWikipedia wiki;

  /** List of errors. */
  private final List<DeadLink> errors;

  /** Text pane where the text is. */
  private final JTextComponent textPane;

  /** Introduction to be displayed. */
  private JLabel labelMessage;

  /**
   * @param wiki Wiki.
   * @param errors List of errors.
   * @param textPane Text pane where the text is.
   */
  public DeadLinkPanel(
      EnumWikipedia wiki,
      List<DeadLink> errors,
      JTextComponent textPane) {
    super(new GridBagLayout(), true);
    this.wiki = wiki;
    this.errors = errors;
    this.textPane = textPane;
    constructContents();
  }

  public void setMessage(String message) {
    if (labelMessage != null) {
      labelMessage.setText(message);
    }
  }

  /**
   * Construct the panel components.
   */
  private void constructContents() {
    GridBagConstraints constraints = new GridBagConstraints(
        0, 0, 1, 1, 1, 0,
        GridBagConstraints.LINE_START, GridBagConstraints.BOTH,
        new Insets(0, 0, 0, 0), 0, 0);

    // Text
    String message = GT._T("The following links seem to be dead:");
    labelMessage = new JLabel(message);
    add(labelMessage, constraints);
    constraints.gridy++;

    // List of detections
    DeadLinkListTableModel modelErrors =
        new DeadLinkListTableModel(wiki, errors, textPane);
    JTable tableErrors = new JTable(modelErrors);
    modelErrors.configureColumnModel(tableErrors.getColumnModel());
    JScrollPane scrollErrors = new JScrollPane(tableErrors);
    scrollErrors.setMinimumSize(new Dimension(500, 100));
    scrollErrors.setPreferredSize(new Dimension(800, 200));
    scrollErrors.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.weighty = 1;
    add(scrollErrors, constraints);
    constraints.gridy++;
  }
}
