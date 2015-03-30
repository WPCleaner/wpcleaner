/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.util.ArrayList;
import java.util.List;

import javax.swing.table.AbstractTableModel;
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
   * List of algorithms.
   */
  private final List<CheckErrorAlgorithm> algorithms;

  /**
   * List of bot algorithms.
   */
  private final List<CheckErrorAlgorithm> botAlgorithms;

  public final static int COLUMN_NUMBER = 0;
  public final static int COLUMN_BOT = COLUMN_NUMBER + 1;
  public final static int COLUMN_DESCRIPTION = COLUMN_BOT + 1;
  public final static int NB_COLUMNS = COLUMN_DESCRIPTION + 1;

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
    addAlgorithm(1);  // Template namespace in template usage
    addAlgorithm(2);  // Article with false <br/>
    addAlgorithm(4);  // <a> tags
    addAlgorithm(6);  // DEFAULTSORT with special letter
    addAlgorithm(7);  // Headlines all start with three "="
    addAlgorithm(9);  // Categories more at one line
    addAlgorithm(11); // HTML named entities
    addAlgorithm(16); // Template with Unicode control characters
    addAlgorithm(17); // Category duplication
    addAlgorithm(18); // Category first letter small
    addAlgorithm(19); // Headlines start with one "="
    addAlgorithm(20); // Symbol for dead
    addAlgorithm(22); // Category with space
    addAlgorithm(25); // Headline hierarchy
    addAlgorithm(27); // Unicode syntax
    addAlgorithm(32); // Double pipe in one link
    addAlgorithm(37); // Title with special letters and no DEFAULTSORT
    addAlgorithm(44); // Headlines with bold
    addAlgorithm(45); // Interwiki double
    addAlgorithm(50); // en dash or em dash
    addAlgorithm(54); // Break in list
    addAlgorithm(57); // Headlines end with colon
    //addAlgorithm(59); // Template value end with break
    addAlgorithm(64); // Link equal to link text
    addAlgorithm(66); // Image description with full <small>
    addAlgorithm(76); // Link with no space
    addAlgorithm(85); // Tag without content
    addAlgorithm(87); // HTML named entities without semicolon
    addAlgorithm(88); // DEFAULTSORT with blank at first position
    addAlgorithm(90); // Internal link written as external link
    addAlgorithm(91); // Interwiki link written as external link
    addAlgorithm(92); // Headline double
    addAlgorithm(524); // Duplicate template argument
  }

  /**
   * Add an algorithm to the list of algorithms that can be fixed by bot.
   * 
   * @param errorNumber Error number.
   */
  private void addAlgorithm(int errorNumber) {
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, errorNumber);
    if ((algorithm != null) &&
        (algorithm.isAvailable()) &&
        CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
      if (errorNumber <= CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
        if (CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
          botAlgorithms.add(algorithm);
        }
      } else {
        if (algorithm.hasSpecialList()) {
          botAlgorithms.add(algorithm);
        }
      }
    }
  }

  /**
   * Configure a column model.
   * 
   * @param model Column model.
   */
  public void configureColumnModel(TableColumnModel model) {
    model.getColumn(COLUMN_BOT).setMinWidth(20);
    model.getColumn(COLUMN_BOT).setPreferredWidth(20);
    model.getColumn(COLUMN_BOT).setMaxWidth(20);
    model.getColumn(COLUMN_BOT).setCellRenderer(
        new BooleanIconCellRenderer("commons-nuvola-apps-kcmsystem.png", null));
    model.getColumn(COLUMN_BOT).setHeaderRenderer(
        new IconCellRenderer("commons-nuvola-apps-kcmsystem.png"));

    model.getColumn(COLUMN_DESCRIPTION).setMinWidth(100);

    model.getColumn(COLUMN_NUMBER).setMinWidth(40);
    model.getColumn(COLUMN_NUMBER).setPreferredWidth(40);
    model.getColumn(COLUMN_NUMBER).setMaxWidth(40);
  }

  /**
   * @return Algorithms.
   */
  public List<CheckErrorAlgorithm> getAlgorithms() {
    return algorithms;
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
    case COLUMN_NUMBER:
      return Integer.class;
    }
    return super.getColumnClass(columnIndex);
  }
}
