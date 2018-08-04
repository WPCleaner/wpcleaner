/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.checkwiki;

import java.util.List;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model for a list of CheckWiki detections.
 */
public class DetectionListTableModel extends AbstractTableModel {

  private static final long serialVersionUID = 1334781606144392233L;

  private List<CheckWikiDetection> detections;

  private final JTextComponent textPane;

  public final static int COLUMN_LOCATION_METHOD = 0;
  public final static int COLUMN_LOCATION = COLUMN_LOCATION_METHOD + 1;
  public final static int COLUMN_ERROR_NUMBER = COLUMN_LOCATION + 1;
  public final static int COLUMN_NOTICE = COLUMN_ERROR_NUMBER + 1;
  public final static int COLUMN_GOTO = COLUMN_NOTICE + 1;

  public final static int NB_COLUMNS_WITHOUT_GOTO = COLUMN_GOTO;
  public final static int NB_COLUMNS_WITH_GOTO = COLUMN_GOTO + 1;

  public DetectionListTableModel(
      List<CheckWikiDetection> detections, JTextComponent textPane) {
    this.detections = detections;
    this.textPane = textPane;
  }

  /**
   * Configure a column model.
   * 
   * @param model Column model.
   */
  public void configureColumnModel(TableColumnModel model) {
    TableColumn column;

    column = model.getColumn(COLUMN_ERROR_NUMBER);
    column.setMinWidth(50);
    column.setPreferredWidth(50);
    column.setMaxWidth(50);

    if (textPane != null) {
      column = model.getColumn(COLUMN_GOTO);
      column.setMinWidth(30);
      column.setPreferredWidth(30);
      column.setMaxWidth(30);
      DetectionRenderer detectionRenderer = new DetectionRenderer(textPane);
      column.setCellEditor(detectionRenderer);
      column.setCellRenderer(detectionRenderer);
    }

    column = model.getColumn(COLUMN_LOCATION);
    column.setMinWidth(60);
    column.setPreferredWidth(60);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_LOCATION_METHOD);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);

    column = model.getColumn(COLUMN_NOTICE);
    column.setMinWidth(100);
    column.setPreferredWidth(300);
  }

  /**
   * @return Number of columns.
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  @Override
  public int getColumnCount() {
    if (textPane != null) {
      return NB_COLUMNS_WITH_GOTO;
    }
    return NB_COLUMNS_WITHOUT_GOTO;
  }

  /**
   * @return Number of rows.
   * @see javax.swing.table.TableModel#getRowCount()
   */
  @Override
  public int getRowCount() {
    return (detections != null) ? detections.size() : 0;
  }

  /**
   * @param rowIndex Row index.
   * @param columnIndex Column index.
   * @return Value at row and column.
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((detections != null) && (rowIndex >= 0) && (rowIndex < detections.size())) {
      CheckWikiDetection detection = detections.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_ERROR_NUMBER:
        return detection.getErrorNumber();
      case COLUMN_GOTO:
        return detection;
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

  /**
   * @param rowIndex Row index.
   * @param columnIndex Column index.
   * @return True if the cell is editable
   * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
   */
  @Override
  public boolean isCellEditable(int rowIndex, int columnIndex) {
    if (columnIndex == COLUMN_GOTO) {
      return (textPane != null);
    }
    return super.isCellEditable(rowIndex, columnIndex);
  }

  /**
   * @param column Column index.
   * @return Name of column.
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  @Override
  public String getColumnName(int column) {
    switch (column) {
    case COLUMN_ERROR_NUMBER:
      return GT._T("Error");
    case COLUMN_GOTO:
      return "";
    case COLUMN_LOCATION:
      return GT._T("Location");
    case COLUMN_LOCATION_METHOD:
      return "";
    case COLUMN_NOTICE:
      return GT._T("Notice");
    }
    return super.getColumnName(column);
  }

  /**
   * @param columnIndex Column index.
   * @return Class of that data in the column.
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  @Override
  public Class<?> getColumnClass(int columnIndex) {
    switch (columnIndex) {
    case COLUMN_ERROR_NUMBER:
      return Integer.class;
    case COLUMN_GOTO:
      return CheckWikiDetection.class;
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
