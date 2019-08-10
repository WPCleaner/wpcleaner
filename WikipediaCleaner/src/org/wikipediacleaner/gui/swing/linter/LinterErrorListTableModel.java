/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.linter;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.gui.swing.component.CopyCellRenderer;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model for a list of Linter errors.
 */
public class LinterErrorListTableModel extends AbstractTableModel {

  /** Serialization */
  private static final long serialVersionUID = 6291117363909928449L;

  /** Wiki */
  private final EnumWikipedia wiki;

  /** List of errors */
  private final List<LinterError> errors;

  /** Text area */
  private final JTextComponent textPane;

  public final static int COLUMN_PAGE = 0;
  public final static int COLUMN_START = COLUMN_PAGE + 1;
  public final static int COLUMN_END = COLUMN_START + 1;
  public final static int COLUMN_TYPE = COLUMN_END + 1;
  public final static int COLUMN_PARAMETERS = COLUMN_TYPE + 1;
  public final static int COLUMN_COPY = COLUMN_PARAMETERS + 1;
  public final static int COLUMN_TEMPLATE = COLUMN_COPY + 1;
  public final static int COLUMN_GOTO = COLUMN_TEMPLATE + 1;

  public final static int NB_COLUMNS = COLUMN_GOTO + 1;

  /**
   * @param wiki Wiki.
   * @param errors List of Linter errors.
   * @param textPane Text area.
   */
  public LinterErrorListTableModel(
      EnumWikipedia wiki,
      List<LinterError> errors,
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

    column = model.getColumn(COLUMN_COPY);
    column.setMinWidth(30);
    column.setPreferredWidth(30);
    column.setMaxWidth(30);
    CopyCellRenderer copyRenderer = new CopyCellRenderer(COLUMN_PARAMETERS);
    column.setCellEditor(copyRenderer);
    column.setCellRenderer(copyRenderer);
    
    column = model.getColumn(COLUMN_END);
    column.setMinWidth(60);
    column.setPreferredWidth(60);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_GOTO);
    column.setMinWidth(30);
    column.setPreferredWidth(30);
    column.setMaxWidth(30);
    LinterErrorRenderer detectionRenderer = new LinterErrorRenderer(textPane, wiki);
    column.setCellEditor(detectionRenderer);
    column.setCellRenderer(detectionRenderer);

    column = model.getColumn(COLUMN_PAGE);
    column.setMinWidth(100);
    column.setPreferredWidth(300);

    column = model.getColumn(COLUMN_PARAMETERS);
    column.setMinWidth(100);
    column.setPreferredWidth(300);

    column = model.getColumn(COLUMN_START);
    column.setMinWidth(60);
    column.setPreferredWidth(60);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_TEMPLATE);
    column.setMinWidth(100);
    column.setPreferredWidth(300);

    column = model.getColumn(COLUMN_TYPE);
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
      LinterError error = errors.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_COPY:
        return error;
      case COLUMN_END:
        return error.getEndOffset();
      case COLUMN_GOTO:
        return error;
      case COLUMN_PAGE:
        return error.getPage();
      case COLUMN_PARAMETERS:
      {
        StringBuilder tmp = new StringBuilder();
        Map<String, String> params = error.getParameters();
        if (params != null) {
          for (Entry<String, String> param : params.entrySet()) {
            if (tmp.length() > 0) {
              tmp.append(";");
            }
            tmp.append(param.getKey());
            tmp.append("=");
            tmp.append(param.getValue());
          }
        }
        return tmp.toString();
      }
      case COLUMN_START:
        return error.getStartOffset();
      case COLUMN_TEMPLATE:
        if (error.getTemplateName() != null) {
          return error.getTemplateName();
        }
        if (error.isMutiPartTemplateBlock()) {
          return GT._T("Multiple templates");
        }
        return "";
      case COLUMN_TYPE:
        return error.getTypeName(wiki.getWikiConfiguration());
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
      return true;
    }
    if (columnIndex == COLUMN_COPY) {
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
    case COLUMN_COPY:
      return "";
    case COLUMN_END:
      return GT._T("End");
    case COLUMN_GOTO:
      return "";
    case COLUMN_PAGE:
      return GT._T("Page");
    case COLUMN_PARAMETERS:
      return GT._T("Parameters");
    case COLUMN_START:
      return GT._T("Start");
    case COLUMN_TEMPLATE:
      return GT._T("Template");
    case COLUMN_TYPE:
      return GT._T("Type");
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
    case COLUMN_COPY:
      return CheckWikiDetection.class;
    case COLUMN_END:
      return Integer.class;
    case COLUMN_GOTO:
      return CheckWikiDetection.class;
    case COLUMN_PARAMETERS:
      return String.class;
    case COLUMN_START:
      return Integer.class;
    case COLUMN_TEMPLATE:
      return String.class;
    case COLUMN_TYPE:
      return String.class;
    }
    return super.getColumnClass(columnIndex);
  }

}
