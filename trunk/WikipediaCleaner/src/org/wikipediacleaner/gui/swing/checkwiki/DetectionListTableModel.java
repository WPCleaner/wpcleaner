/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.checkwiki;

import java.util.List;

import javax.swing.table.AbstractTableModel;

import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model for a list of CheckWiki detections.
 */
public class DetectionListTableModel extends AbstractTableModel {

  private static final long serialVersionUID = 1334781606144392233L;

  private List<CheckWikiDetection> detections;

  public final static int COLUMN_LOCATION_METHOD = 0;
  public final static int COLUMN_LOCATION = COLUMN_LOCATION_METHOD + 1;
  public final static int COLUMN_ERROR_NUMBER = COLUMN_LOCATION + 1;
  public final static int COLUMN_NOTICE = COLUMN_ERROR_NUMBER + 1;
  public final static int NB_COLUMNS = COLUMN_NOTICE + 1;

  public DetectionListTableModel(
      List<CheckWikiDetection> detections) {
    this.detections = detections;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount() {
    return (detections != null) ? detections.size() : 0;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((detections != null) && (rowIndex >= 0) && (rowIndex < detections.size())) {
      CheckWikiDetection detection = detections.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_ERROR_NUMBER:
        return detection.getErrorNumber();
      case COLUMN_LOCATION:
        return detection.getLocation();
      case COLUMN_LOCATION_METHOD:
        return detection.getLocationMethod();
      case COLUMN_NOTICE:
        return detection.getDetection();
      }
    }
    return null;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  @Override
  public String getColumnName(int column) {
    switch (column) {
    case COLUMN_ERROR_NUMBER:
      return GT._("Error");
    case COLUMN_LOCATION:
      return GT._("Location");
    case COLUMN_LOCATION_METHOD:
      return "";
    case COLUMN_NOTICE:
      return GT._("Notice");
    }
    return super.getColumnName(column);
  }

  /* (non-Javadoc)
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  @Override
  public Class<?> getColumnClass(int columnIndex) {
    switch (columnIndex) {
    case COLUMN_ERROR_NUMBER:
      return Integer.class;
    case COLUMN_LOCATION:
      return Integer.class;
    case COLUMN_LOCATION_METHOD:
      return String.class;
    case COLUMN_NOTICE:
      return String.class;
    }
    return super.getColumnClass(columnIndex);
  }

}
