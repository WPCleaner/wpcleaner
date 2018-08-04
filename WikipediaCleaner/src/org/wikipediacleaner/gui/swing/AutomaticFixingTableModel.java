/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.table.AbstractTableModel;

import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model to manage Automatic fixing expressions.
 */
class AutomaticFixingTableModel extends AbstractTableModel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -6615379596445669242L;

  /**
   * List of automatic fixing expressions.
   */
  private List<AutomaticFixing> data;

  public final static int COLUMN_FROM = 0;
  public final static int COLUMN_TO = COLUMN_FROM + 1;
  public final static int COLUMN_REGEX = COLUMN_TO + 1;
  public final static int NB_COLUMNS = COLUMN_REGEX + 1;

  /**
   * @param data List of automatic fixing expressions.
   */
  public AutomaticFixingTableModel(List<AutomaticFixing> data) {
    this.data = data;
  }

  /**
   * @param rowIndex Row index.
   * @return Automatic fixing at the designated row.
   */
  public AutomaticFixing getAutomaticFixing(int rowIndex) {
    if ((data == null) || (rowIndex < 0) || (rowIndex >= data.size())) {
      return null;
    }
    return data.get(rowIndex);
  }

  /**
   * Set the list of automatic fixing expressions.
   * 
   * @param data List of automatic fixing expressions.
   */
  public void setData(List<AutomaticFixing> data) {
    if (data == null) {
      this.data = null;
    } else {
      this.data = new ArrayList<AutomaticFixing>(data);
    }
    sortAutomaticFixing();
    fireTableDataChanged();
  }

  /**
   * @return List of automatic fixing expressions.
   */
  public List<AutomaticFixing> getData() {
    if (data == null) {
      return null;
    }
    return Collections.unmodifiableList(data);
  }

  /**
   * Add an automatic fixing expression.
   * 
   * @param element Automatic fixing expression.
   */
  public void addAutomaticFixing(AutomaticFixing element) {
    if (element == null) {
      return;
    }
    if (data == null) {
      data = new ArrayList<AutomaticFixing>();
    }
    data.add(element);
    sortAutomaticFixing();
    fireTableDataChanged();
  }

  /**
   * Remove an automatic fixing expression.
   * 
   * @param element Automatic fixing expression.
   */
  public void removeAutomaticFixing(AutomaticFixing element) {
    if (data == null) {
      return;
    }
    data.remove(element);
    sortAutomaticFixing();
    fireTableDataChanged();
  }

  /**
   * Sort automatic fixing expressions.
   */
  private void sortAutomaticFixing() {
    if (data == null) {
      return;
    }
    Collections.sort(data);
  }

  /**
   * @return Number of rows in the table.
   * @see javax.swing.table.TableModel#getRowCount()
   */
  @Override
  public int getRowCount() {
    if (data != null) {
      return data.size();
    }
    return 0;
  }

  /**
   * @return Number of columns in the table.
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  @Override
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /**
   * @param rowIndex Row index.
   * @param columnIndex Column index.
   * @return Value in the designated cell.
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((data == null) ||
        (rowIndex < 0) || (rowIndex >= getRowCount()) ||
        (columnIndex < 0) || (columnIndex >= getColumnCount())) {
      return null;
    }
    AutomaticFixing fixing = data.get(rowIndex);
    if (fixing == null) {
      return null;
    }
    switch (columnIndex) {
    case COLUMN_FROM:
      return fixing.getOriginalText();
    case COLUMN_TO:
      return fixing.getReplacementText();
    case COLUMN_REGEX:
      return fixing.getRegex();
    }
    return null;
  }

  /**
   * @param column Column index.
   * @return Name of the column.
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  @Override
  public String getColumnName(int column) {
    switch (column) {
    case COLUMN_FROM:
      return GT._T("Original text");
    case COLUMN_TO:
      return GT._T("Replacement text");
    case COLUMN_REGEX:
      return GT._T("regex");
    }
    return super.getColumnName(column);
  }

  /**
   * @param columnIndex Column index.
   * @return Type of data in the column.
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  @Override
  public Class<?> getColumnClass(int columnIndex) {
    switch (columnIndex) {
    case COLUMN_FROM:
      return String.class;
    case COLUMN_TO:
      return String.class;
    case COLUMN_REGEX:
      return Boolean.class;
    }
    return super.getColumnClass(columnIndex);
  }
}
