/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.data.Page;
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
   * Format for time stamp.
   */
  private final DateFormat timestampFormat;

  /**
   * Maximum number of changes to keep in the history.
   */
  private int maxChanges;

  public final static int COLUMN_RC_ID = 0;
  public final static int COLUMN_FLAGS = COLUMN_RC_ID + 1;
  public final static int COLUMN_TIMESTAMP = COLUMN_FLAGS + 1;
  public final static int COLUMN_USER = COLUMN_TIMESTAMP + 1;
  public final static int COLUMN_TITLE = COLUMN_USER + 1;
  public final static int NB_COLUMNS = COLUMN_TITLE + 1;

  /**
   * Constructor.
   * 
   * @param recentChanges Initial list of recent changes.
   */
  public RecentChangesTableModel(List<RecentChange> recentChanges) {
    super();
    this.timestampFormat = new SimpleDateFormat("HH:mm:ss");
    this.maxChanges = 500;
    this.recentChanges = new LinkedList<RecentChange>();
    if (recentChanges != null) {
      this.recentChanges.addAll(recentChanges);
    }
    cleanUpList();
  }

  /**
   * @return List of all recent changes.
   */
  public List<RecentChange> getRecentChanges() {
    List<RecentChange> result = new ArrayList<RecentChange>(recentChanges.size());
    result.addAll(recentChanges);
    return result;
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
   * Add one recent change to the list.
   * 
   * @param rc Recent change.
   */
  public void addRecentChange(RecentChange rc) {
    if (!recentChanges.contains(rc)) {
      recentChanges.add(rc);
      cleanUpList();
    }
  }

  /**
   * Remove all recent changes for a given title.
   * 
   * @param title Title.
   */
  public void removeRecentChanges(String title) {
    Iterator<RecentChange> itRC = recentChanges.iterator();
    while (itRC.hasNext()) {
      RecentChange rc = itRC.next();
      if (Page.areSameTitle(title, rc.getTitle())) {
        itRC.remove();
      }
    }
    cleanUpList();
  }

  /**
   * Check if there are recent changes for a given title.
   * 
   * @param title Title.
   * @return True if there are recent changes for the title.
   */
  public boolean containsRecentChange(String title) {
    for (RecentChange rc : recentChanges) {
      if (Page.areSameTitle(title, rc.getTitle())) {
        return true;
      }
    }
    return false;
  }

  /**
   * Configure a column model.
   * 
   * @param model Column model.
   */
  public void configureColumnModel(TableColumnModel model) {
    model.getColumn(RecentChangesTableModel.COLUMN_FLAGS).setMinWidth(40);
    model.getColumn(RecentChangesTableModel.COLUMN_FLAGS).setMaxWidth(40);
    model.getColumn(RecentChangesTableModel.COLUMN_RC_ID).setMinWidth(80);
    model.getColumn(RecentChangesTableModel.COLUMN_RC_ID).setPreferredWidth(80);
    model.getColumn(RecentChangesTableModel.COLUMN_RC_ID).setMaxWidth(100);
    model.getColumn(RecentChangesTableModel.COLUMN_TIMESTAMP).setMinWidth(60);
    model.getColumn(RecentChangesTableModel.COLUMN_TIMESTAMP).setMaxWidth(60);
    model.getColumn(RecentChangesTableModel.COLUMN_TITLE).setMinWidth(100);
    model.getColumn(RecentChangesTableModel.COLUMN_TITLE).setPreferredWidth(200);
    model.getColumn(RecentChangesTableModel.COLUMN_USER).setMinWidth(100);
    model.getColumn(RecentChangesTableModel.COLUMN_USER).setPreferredWidth(100);
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
  @Override
  public int getRowCount() {
    return recentChanges.size();
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
   * @param rowIndex Row index.
   * @param columnIndex Column index.
   * @return Value at row and column.
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((rowIndex >= 0) && (rowIndex < recentChanges.size())) {
      RecentChange rc = recentChanges.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_FLAGS:
        if (RecentChange.TYPE_LOG.equals(rc.getType())) {
          if (RecentChange.LOG_TYPE_BLOCK.equals(rc.getLogType())) {
            return "B";
          }
          if (RecentChange.LOG_TYPE_DELETE.equals(rc.getLogType())) {
            return "D";
          }
          if (RecentChange.LOG_TYPE_MOVE.equals(rc.getLogType())) {
            return "M";
          }
          if (RecentChange.LOG_TYPE_NEWUSERS.equals(rc.getLogType())) {
            return "User";
          }
          if (RecentChange.LOG_TYPE_PROTECT.equals(rc.getLogType())) {
            return "P";
          }
          if (RecentChange.LOG_TYPE_UPLOAD.equals(rc.getLogType())) {
            return "Upld";
          }
        }
        return
            (rc.isNew() ? "N" : "") +
            (rc.isMinor() ? "m" : "") +
            (rc.isBot() ? "b" : "") +
            (rc.isRedirect() ? "R" : "");
      case COLUMN_RC_ID:
        return rc.getId();
      case COLUMN_TIMESTAMP:
        synchronized (timestampFormat) {
          return timestampFormat.format(rc.getTimestamp()); 
        }
      case COLUMN_TITLE:
        return rc.getTitle();
      case COLUMN_USER:
        return rc.getUser();
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
      return GT._T("Id");
    case COLUMN_TIMESTAMP:
      return "Time";
    case COLUMN_TITLE:
      return GT._T("Title");
    case COLUMN_USER:
      return GT._T("User");
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
    case COLUMN_FLAGS:
      return String.class;
    case COLUMN_RC_ID:
      return Integer.class;
    case COLUMN_TIMESTAMP:
      return String.class;
    case COLUMN_TITLE:
      return String.class;
    case COLUMN_USER:
      return String.class;
    }
    return super.getColumnClass(columnIndex);
  }
}
