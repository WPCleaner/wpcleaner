/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;

import org.wikipediacleaner.api.data.ProgressionValue;


/**
 * A cell renderer for ProgressionValue.
 */
public class ProgressionValueCellRenderer extends DefaultTableCellRenderer {

  private static final long serialVersionUID = -6288144000371007974L;

  /**
   * Constructor.
   */
  public ProgressionValueCellRenderer() {
    super();
    setHorizontalAlignment(SwingConstants.RIGHT);
  }

  /**
   * Returns the table cell renderer for ProgressionValue.
   * 
   * @param table  the <code>JTable</code>
   * @param value  the value to assign to the cell at
   *      <code>[row, column]</code>
   * @param isSelected true if cell is selected
   * @param hasFocus true if cell has focus
   * @param row  the row of the cell to render
   * @param column the column of the cell to render
   * @return the default table cell renderer
   * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
   */
  @Override
  public Component getTableCellRendererComponent(
      JTable table, Object value,
      boolean isSelected, boolean hasFocus,
      int row, int column) {
    Component component = super.getTableCellRendererComponent(
        table, value, isSelected, hasFocus, row, column);
    if (value instanceof ProgressionValue) {
      ProgressionValue progression = (ProgressionValue) value;
      int status = progression.getStatus();
      if (status < 0) {
        if (isSelected) {
          component.setBackground(table.getSelectionBackground());
          component.setForeground(Color.GREEN);
        } else {
          component.setBackground(Color.GREEN);
          component.setForeground(table.getForeground());
        }
      } else if (status > 0) {
        if (isSelected) {
          component.setBackground(table.getSelectionBackground());
          component.setForeground(Color.RED);
        } else {
          component.setBackground(Color.RED);
          component.setForeground(table.getForeground());
        }
      } else {
        if (isSelected) {
          component.setBackground(table.getSelectionBackground());
          if ((progression.getCurrent() != null) && (progression.getGoal() == null)) {
            component.setForeground(Color.GRAY);
          } else {
            component.setForeground(table.getSelectionForeground());
          }
        } else {
          component.setBackground(table.getBackground());
          if ((progression.getCurrent() != null) && (progression.getGoal() == null)) {
            component.setForeground(Color.GRAY);
          } else {
            component.setForeground(table.getForeground());
          }
        }
      }
    }
    return component;
  }

}
