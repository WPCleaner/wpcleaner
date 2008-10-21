/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing;

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

  /* (non-Javadoc)
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
