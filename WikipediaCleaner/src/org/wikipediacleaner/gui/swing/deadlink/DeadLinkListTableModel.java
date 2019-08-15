/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.deadlink;

import java.util.List;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.contents.Interval;
import org.wikipediacleaner.gui.swing.component.CopyCellRenderer;
import org.wikipediacleaner.gui.swing.component.GoToExternalRenderer;
import org.wikipediacleaner.gui.swing.component.GoToIntervalRenderer;
import org.wikipediacleaner.gui.swing.component.GoToPageRenderer;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model for a list of dead links.
 */
public class DeadLinkListTableModel extends AbstractTableModel {

  /** Serialization */
  private static final long serialVersionUID = 6291117363909928449L;

  /** Wiki */
  private final EnumWikipedia wiki;

  /** List of errors */
  private final List<DeadLink> errors;

  /** Text area */
  private final JTextComponent textPane;

  public final static int COLUMN_PAGE = 0;
  public final static int COLUMN_START = COLUMN_PAGE + 1;
  public final static int COLUMN_END = COLUMN_START + 1;
  public final static int COLUMN_LINK = COLUMN_END + 1;
  public final static int COLUMN_LINK_COPY = COLUMN_LINK + 1;
  public final static int COLUMN_LINK_VIEW = COLUMN_LINK_COPY + 1;
  public final static int COLUMN_STATUS = COLUMN_LINK_VIEW + 1;
  public final static int COLUMN_STATUS_TEXT = COLUMN_STATUS + 1;
  public final static int COLUMN_GOTO = COLUMN_STATUS_TEXT + 1;

  public final static int NB_COLUMNS = COLUMN_GOTO + 1;

  /**
   * @param wiki Wiki.
   * @param errors List of dead links.
   * @param textPane Text area.
   */
  public DeadLinkListTableModel(
      EnumWikipedia wiki,
      List<DeadLink> errors,
      JTextComponent textPane) {
    this.wiki = wiki;
    this.errors = errors;
    this.textPane = textPane;
  }

  /**
   * Configure a column model.
   * 
   * @param model Column model.
   */
  public void configureColumnModel(TableColumnModel model) {
    TableColumn column;

    column = model.getColumn(COLUMN_END);
    column.setMinWidth(60);
    column.setPreferredWidth(60);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_GOTO);
    column.setMinWidth(30);
    column.setPreferredWidth(30);
    column.setMaxWidth(30);
    if (textPane != null) {
      GoToIntervalRenderer renderer = new GoToIntervalRenderer(textPane);
      column.setCellEditor(renderer);
      column.setCellRenderer(renderer);
    } else {
      GoToPageRenderer renderer = new GoToPageRenderer(wiki);
      column.setCellEditor(renderer);
      column.setCellRenderer(renderer);
    }

    column = model.getColumn(COLUMN_LINK);
    column.setMinWidth(100);
    column.setPreferredWidth(300);

    column = model.getColumn(COLUMN_LINK_COPY);
    column.setMinWidth(30);
    column.setPreferredWidth(30);
    column.setMaxWidth(30);
    CopyCellRenderer copyRenderer = new CopyCellRenderer(COLUMN_LINK);
    column.setCellEditor(copyRenderer);
    column.setCellRenderer(copyRenderer);

    column = model.getColumn(COLUMN_LINK_VIEW);
    column.setMinWidth(30);
    column.setPreferredWidth(30);
    column.setMaxWidth(30);
    GoToExternalRenderer viewRenderer = new GoToExternalRenderer();
    column.setCellEditor(viewRenderer);
    column.setCellRenderer(viewRenderer);

    column = model.getColumn(COLUMN_PAGE);
    column.setMinWidth(100);
    column.setPreferredWidth(300);

    column = model.getColumn(COLUMN_START);
    column.setMinWidth(60);
    column.setPreferredWidth(60);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_STATUS);
    column.setMinWidth(60);
    column.setPreferredWidth(100);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_STATUS_TEXT);
    column.setMinWidth(100);
    column.setPreferredWidth(300);
  }

  /**
   * @return Number of columns.
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  @Override
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /**
   * @return Number of rows.
   * @see javax.swing.table.TableModel#getRowCount()
   */
  @Override
  public int getRowCount() {
    return (errors != null) ? errors.size() : 0;
  }

  /**
   * @param rowIndex Row index.
   * @param columnIndex Column index.
   * @return Value at row and column.
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((errors != null) && (rowIndex >= 0) && (rowIndex < errors.size())) {
      DeadLink error = errors.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_END:
        return (error != null) ? error.getEndIndex() : null;
      case COLUMN_GOTO:
        return (textPane != null) ? error : (error != null) ? error.getPage() : null;
      case COLUMN_LINK:
      case COLUMN_LINK_COPY:
      case COLUMN_LINK_VIEW:
        return (error != null) ? error.getLink().getLink() : null;
      case COLUMN_PAGE:
        return (error != null) ? error.getPage() : null;
      case COLUMN_START:
        return (error != null) ? error.getBeginIndex() : null;
      case COLUMN_STATUS:
        return (error != null) ? error.getStatus() : null;
      case COLUMN_STATUS_TEXT:
        return (error != null) ? error.getStatusText() : null;
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
    if ((columnIndex == COLUMN_GOTO) ||
        (columnIndex == COLUMN_LINK_COPY) ||
        (columnIndex == COLUMN_LINK_VIEW)) {
      return true;
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
    case COLUMN_END:
      return GT._T("End");
    case COLUMN_GOTO:
      return "";
    case COLUMN_LINK:
      return GT._T("Link");
    case COLUMN_LINK_COPY:
    case COLUMN_LINK_VIEW:
      return "";
    case COLUMN_PAGE:
      return GT._T("Page");
    case COLUMN_START:
      return GT._T("Start");
    case COLUMN_STATUS:
      return "";
    case COLUMN_STATUS_TEXT:
      return GT._T("Status");
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
    case COLUMN_END:
      return Integer.class;
    case COLUMN_GOTO:
      return (textPane != null) ? Interval.class : String.class;
    case COLUMN_LINK:
    case COLUMN_LINK_COPY:
    case COLUMN_LINK_VIEW:
      return String.class;
    case COLUMN_START:
      return Integer.class;
    case COLUMN_STATUS:
      return Integer.class;
    case COLUMN_STATUS_TEXT:
      return String.class;
    }
    return super.getColumnClass(columnIndex);
  }

}
