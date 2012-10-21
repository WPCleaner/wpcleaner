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

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.swing.table.AbstractTableModel;

import org.wikipediacleaner.api.data.RecentChange;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model for Recent Changes.
 */
public class RecentChangesTableModel extends AbstractTableModel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 6449639241165534424L;

  /**
   * Recent changes.
   */
  private final List<RecentChange> recentChanges;

  /**
   * Maximum number of changes to keep in the history.
   */
  private int maxChanges;

  public final static int COLUMN_RC_ID = 0;
  public final static int COLUMN_FLAGS = COLUMN_RC_ID + 1;
  public final static int COLUMN_TITLE = COLUMN_FLAGS + 1;
  public final static int NB_COLUMNS = COLUMN_TITLE + 1;

  /**
   * Constructor.
   * 
   * @param recentChanges Initial list of recent changes.
   */
  public RecentChangesTableModel(List<RecentChange> recentChanges) {
    super();
    this.maxChanges = 500;
    this.recentChanges = new LinkedList<RecentChange>();
    if (recentChanges != null) {
      this.recentChanges.addAll(recentChanges);
    }
    cleanUpList();
  }

  /**
   * Add new recent changes to the list.
   * 
   * @param newRC New recent changes.
   */
  public void addRecentChanges(List<RecentChange> newRC) {
    if ((newRC != null) && (!newRC.isEmpty())) {
      for (RecentChange rc : newRC) {
        if (!recentChanges.contains(rc)) {
          recentChanges.add(rc);
        }
      }
      cleanUpList();
    }
  }

  /**
   * Clean up list of recent changes.
   */
  private void cleanUpList() {
    Collections.sort(recentChanges);
    while (recentChanges.size() > maxChanges) {
      recentChanges.remove(recentChanges.size() - 1);
    }
    fireTableDataChanged();
  }

  /**
   * @return Number of rows.
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount() {
    return recentChanges.size();
  }

  /**
   * @return Number of columns.
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /**
   * @param rowIndex
   * @param columnIndex
   * @return
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((rowIndex >= 0) && (rowIndex < recentChanges.size())) {
      RecentChange rc = recentChanges.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_FLAGS:
        return (rc.isNew() ? "N" : "") + (rc.isMinor() ? "m" : "") + (rc.isBot() ? "b" : "");
      case COLUMN_RC_ID:
        return rc.getId();
      case COLUMN_TITLE:
        return rc.getTitle();
      }
    }
    return null;
  }

  /**
   * @param column Column index.
   * @return Name of column.
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  @Override
  public String getColumnName(int column) {
    switch (column) {
    case COLUMN_FLAGS:
      return "Nmb";
    case COLUMN_RC_ID:
      return GT._("Id");
    case COLUMN_TITLE:
      return GT._("Title");
    }
    return super.getColumnName(column);
  }

  @Override
  public Class<?> getColumnClass(int columnIndex) {
    switch (columnIndex) {
    case COLUMN_FLAGS:
      return String.class;
    case COLUMN_RC_ID:
      return Integer.class;
    case COLUMN_TITLE:
      return String.class;
    }
    return super.getColumnClass(columnIndex);
  }
}
