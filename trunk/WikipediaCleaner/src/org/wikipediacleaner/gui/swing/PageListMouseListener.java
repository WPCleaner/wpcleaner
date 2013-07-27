/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
