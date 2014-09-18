/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.checkwiki;

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

import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.i18n.GT;


/**
 * A panel for a list of Check Wiki detections.
 */
public class DetectionPanel extends JPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -3263240015498265616L;

  /**
   * List of detections.
   */
  private final List<CheckWikiDetection> detections;

  public DetectionPanel(List<CheckWikiDetection> detections) {
    super(new GridBagLayout(), true);
    this.detections = detections;
    constructContents();
  }

  private void constructContents() {
    GridBagConstraints constraints = new GridBagConstraints(
        0, 0, 1, 1, 1, 0,
        GridBagConstraints.LINE_START, GridBagConstraints.BOTH,
        new Insets(0, 0, 0, 0), 0, 0);

    // Text
    String message = GT._("The following errors are currently detected by CheckWiki:");
    JLabel label = new JLabel(message);
    add(label, constraints);
    constraints.gridy++;

    // List of detections
    DetectionListTableModel modelDetections = new DetectionListTableModel(detections);
    JTable tableDetections = new JTable(modelDetections);
    modelDetections.configureColumnModel(tableDetections.getColumnModel());
    JScrollPane scrollDetections = new JScrollPane(tableDetections);
    scrollDetections.setMinimumSize(new Dimension(500, 100));
    scrollDetections.setPreferredSize(new Dimension(500, 200));
    scrollDetections.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.weighty = 1;
    add(scrollDetections, constraints);
    constraints.gridy++;
  }
}
