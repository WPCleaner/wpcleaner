/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.component.BooleanIconCellRenderer;
import org.wikipediacleaner.gui.swing.component.IconCellRenderer;
import org.wikipediacleaner.i18n.GT;


/**
 * A table model for Check Wiki errors in the bot tools.
 */
public class BotCWTableModel extends AbstractTableModel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 1124626680835189828L;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * List of available algorithms.
   */
  private final List<CheckErrorAlgorithm> algorithms;

  /**
   * List of bot algorithms.
   */
  private final List<CheckErrorAlgorithm> botAlgorithms;

  /**
   * List of algorithms selected for fixing errors.
   */
  private final List<CheckErrorAlgorithm> fixAlgorithms;

  /**
   * List of algorithms selected for listing errors.
   */
  private final List<CheckErrorAlgorithm> listAlgorithms;

  public final static int COLUMN_NUMBER = 0;
  public final static int COLUMN_FIX = COLUMN_NUMBER + 1;
  public final static int COLUMN_LIST = COLUMN_FIX + 1;
  public final static int COLUMN_DESCRIPTION = COLUMN_LIST + 1;
  public final static int COLUMN_BOT = COLUMN_DESCRIPTION + 1;
  public final static int NB_COLUMNS = COLUMN_BOT + 1;

  /**
   * @param wiki Wiki.
   */
  public BotCWTableModel(EnumWikipedia wiki) {
    this.wiki = wiki;
    List<CheckErrorAlgorithm> tmpAlgorithms = CheckErrorAlgorithms.getAlgorithms(wiki);
    algorithms = new ArrayList<CheckErrorAlgorithm>();
    for (CheckErrorAlgorithm algorithm : tmpAlgorithms) {
      if (algorithm.isAvailable() &&
          CheckErrorAlgorithms.isAlgorithmActive(wiki, algorithm.getErrorNumber())) {
        algorithms.add(algorithm);
      }
    }
    botAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    fixAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    listAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    addAlgorithm(1   , true , false);  // Template namespace in template usage
    addAlgorithm(2   , true , false);  // Article with false <br/>
    addAlgorithm(4   , true , false);  // <a> tags
    addAlgorithm(6   , true , false);  // DEFAULTSORT with special letter
    addAlgorithm(7   , false, false);  // Headlines all start with three "="
    addAlgorithm(9   , true , false);  // Categories more at one line
    addAlgorithm(11  , true , false); // HTML named entities
    addAlgorithm(16  , true , false); // Template with Unicode control characters
    addAlgorithm(17  , true , false); // Category duplication
    addAlgorithm(18  , true , false); // Category first letter small
    addAlgorithm(19  , false, false); // Headlines start with one "="
    addAlgorithm(20  , true , false); // Symbol for dead
    addAlgorithm(22  , true , false); // Category with space
    addAlgorithm(25  , false, false); // Headline hierarchy
    addAlgorithm(27  , false, false); // Unicode syntax
    addAlgorithm(32  , false, false); // Double pipe in one link
    addAlgorithm(37  , true , false); // Title with special letters and no DEFAULTSORT
    addAlgorithm(44  , false, false); // Headlines with bold
    addAlgorithm(45  , true , false); // Interwiki double
    addAlgorithm(50  , true , false); // en dash or em dash
    addAlgorithm(54  , true , false); // Break in list
    addAlgorithm(57  , false, false); // Headlines end with colon
    addAlgorithm(59  , false, false); // Template value end with break
    addAlgorithm(64  , true , false); // Link equal to link text
    addAlgorithm(66  , false, false); // Image description with full <small>
    addAlgorithm(76  , false, false); // Link with no space
    addAlgorithm(85  , true , false); // Tag without content
    addAlgorithm(87  , false, false); // HTML named entities without semicolon
    addAlgorithm(88  , true , false); // DEFAULTSORT with blank at first position
    addAlgorithm(90  , true , false); // Internal link written as external link
    addAlgorithm(91  , true , false); // Interwiki link written as external link
    addAlgorithm(92  , true , false); // Headline double
    addAlgorithm(524 , true , false); // Duplicate template argument
  }

  /**
   * Add an algorithm to the list of algorithms that can be fixed by bot.
   * 
   * @param errorNumber Error number.
   * @param fix True if algorithm should be used by default for fixing errors.
   * @param list True if algorithm should be used by default for listing errors.
   */
  private void addAlgorithm(int errorNumber, boolean fix, boolean list) {
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, errorNumber);
    if ((algorithm != null) &&
        (algorithm.isAvailable()) &&
        CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
      boolean active = false;
      if (errorNumber <= CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
        if (CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
          active = true;
        }
      } else {
        if (algorithm.hasSpecialList()) {
          active = true;
        }
      }
      if (active) {
        botAlgorithms.add(algorithm);
        if (fix) {
          fixAlgorithms.add(algorithm);
        }
        if (list) {
          listAlgorithms.add(algorithm);
        }
      }
    }
  }

  /**
   * Configure a column model.
   * 
   * @param table Table for which the column model should be configured.
   */
  public void configureColumnModel(JTable table) {
    if (table == null) {
      return;
    }
    TableColumnModel model = table.getColumnModel();
    TableColumn column = null;

    column = model.getColumn(COLUMN_BOT);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);
    column.setCellRenderer(
        new BooleanIconCellRenderer("commons-approve-icon.png", null));
    column.setHeaderRenderer(
        new IconCellRenderer("commons-nuvola-apps-kcmsystem.png"));

    column = model.getColumn(COLUMN_DESCRIPTION);
    column.setMinWidth(100);

    column = model.getColumn(COLUMN_FIX);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);
    column.setHeaderRenderer(
        new IconCellRenderer("commons-nuvola-apps-kcmsystem.png"));

    column = model.getColumn(COLUMN_LIST);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);
    column.setHeaderRenderer(
        new IconCellRenderer("gnome-logviewer.png"));

    column = model.getColumn(COLUMN_NUMBER);
    column.setMinWidth(40);
    column.setPreferredWidth(40);
    column.setMaxWidth(40);

    table.addMouseListener(EventHandler.create(
        MouseListener.class, this, "mouseClicked", "", "mouseClicked"));
  }

  /**
   * Called when user clicks on the table.
   * 
   * @param e Mouse event.
   */
  public void mouseClicked(MouseEvent e) {
    if ((e != null) && (e.getClickCount() == 1)) {
      JTable target = (JTable) e.getSource();
      int row = target.getSelectedRow();
      int column = target.getSelectedColumn();
      CheckErrorAlgorithm algorithm = getAlgorithm(row);
      List<CheckErrorAlgorithm> list = null;
      switch (column) {
      case COLUMN_FIX:
        list = fixAlgorithms;
        break;
      case COLUMN_LIST:
        list = listAlgorithms;
        break;
      }
      if (list != null) {
        if (list.contains(algorithm)) {
          list.remove(algorithm);
        } else {
          list.add(algorithm);
        }
        fireTableCellUpdated(row, column);
      }
    }
  }

  /**
   * @return Algorithms.
   */
  public List<CheckErrorAlgorithm> getAlgorithms() {
    return algorithms;
  }

  /**
   * @return Algorithms selected for fixing errors.
   */
  public List<CheckErrorAlgorithm> getFixAlgorithms() {
    return fixAlgorithms;
  }

  /**
   * @return Algorithms selected for listing errors.
   */
  public List<CheckErrorAlgorithm> getListAlgorithms() {
    return listAlgorithms;
  }

  /**
   * @param rowIndex Row index.
   * @return Algorithm.
   */
  public CheckErrorAlgorithm getAlgorithm(int rowIndex) {
    return algorithms.get(rowIndex);
  }

  /**
   * @param rowIndex Row index.
   * @return true if the algorithm is for bot.
   */
  public boolean isBotAlgorithm(int rowIndex) {
    CheckErrorAlgorithm algorithm = getAlgorithm(rowIndex);
    return botAlgorithms.contains(algorithm);
  }

  /**
   * @return Number of rows.
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount() {
    return (algorithms != null) ? algorithms.size() : 0;
  }

  /**
   * @return Number of columns.
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /**
   * @param rowIndex Row index.
   * @param columnIndex Column index.
   * @return Value at row and column.
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((rowIndex >= 0) && (rowIndex < algorithms.size())) {
      CheckErrorAlgorithm algorithm = algorithms.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_BOT:
        return Boolean.valueOf(botAlgorithms.contains(algorithm));
      case COLUMN_DESCRIPTION:
        return algorithm.getShortDescriptionReplaced();
      case COLUMN_FIX:
        return Boolean.valueOf(fixAlgorithms.contains(algorithm));
      case COLUMN_LIST:
        return Boolean.valueOf(listAlgorithms.contains(algorithm));
      case COLUMN_NUMBER:
        return Integer.valueOf(algorithm.getErrorNumber());
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
    case COLUMN_BOT:
      return "Bot";
    case COLUMN_DESCRIPTION:
      return GT._("Description");
    case COLUMN_FIX:
      return "Fix";
    case COLUMN_LIST:
      return "List";
    case COLUMN_NUMBER:
      return GT._("N°");
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
    case COLUMN_BOT:
      return Boolean.class;
    case COLUMN_DESCRIPTION:
      return String.class;
    case COLUMN_FIX:
      return Boolean.class;
    case COLUMN_LIST:
      return Boolean.class;
    case COLUMN_NUMBER:
      return Integer.class;
    }
    return super.getColumnClass(columnIndex);
  }
}
