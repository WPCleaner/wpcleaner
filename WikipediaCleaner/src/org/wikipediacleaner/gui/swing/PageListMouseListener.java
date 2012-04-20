/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JTable;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;


/**
 * Mouse listener on a list of pages.
 */
public class PageListMouseListener extends MouseAdapter {

  /**
   * @param e Event.
   * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseClicked(MouseEvent e) {
    if ((!e.isConsumed()) &&
        (e.getButton() == MouseEvent.BUTTON1) &&
        (e.getClickCount() == 2)) {
      if (e.getSource() instanceof JTable) {
        JTable table = (JTable) e.getSource();
        if (table.getModel() instanceof PageListTableModel) {
          PageListTableModel model = (PageListTableModel) table.getModel();
          int column = table.columnAtPoint(e.getPoint());
          int row = table.rowAtPoint(e.getPoint());
          if ((column >= 0) && (row >= 0)) {
            row = Utilities.convertRowIndexToModel(table, row);
            Page page = model.getPage(row);
            if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
              Controller.runDisambiguationAnalysis(page.getTitle(), page.getWikipedia());
            } else {
              Controller.runFullAnalysis(page.getTitle(), null, page.getWikipedia());
            }
            e.consume();
          }
        }
      }
    }
    super.mouseClicked(e);
  }
}
