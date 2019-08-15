/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.HashMap;

import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Cell renderer and editor for a go to button for a page.
 */
public class GoToPageRenderer extends AbstractCellEditor implements
    TableCellRenderer, TableCellEditor {

  /** Serialization */
  private static final long serialVersionUID = 9192058472932969543L;

  /** Maps of all the buttons. */
  private HashMap<Object, JButton> buttons;

  /** Wiki */
  private final EnumWikipedia wiki;

  /**
   * @param wiki Wiki.
   */
  public GoToPageRenderer(EnumWikipedia wiki) {
    buttons = new HashMap<Object, JButton>();
    this.wiki = wiki;
  }

  /**
   * @return Value in the editor.
   * @see javax.swing.CellEditor#getCellEditorValue()
   */
  @Override
  public Object getCellEditorValue() {
    return null;
  }

  /**
   * @param table Table.
   * @param value Value in the cell.
   * @param isSelected True if cell is selected.
   * @param row Row number.
   * @param column Column number.
   * @return Component to edit the cell.
   * @see javax.swing.table.TableCellEditor#getTableCellEditorComponent(javax.swing.JTable, java.lang.Object, boolean, int, int)
   */
  @Override
  public Component getTableCellEditorComponent(
      JTable table, Object value,
      boolean isSelected, int row, int column) {
    return getButton(value);
  }

  /**
   * @param table Table.
   * @param value Value in the cell.
   * @param isSelected True if cell is selected.
   * @param hasFocus True if cell has focus.
   * @param row Row number.
   * @param column Column number.
   * @return Component to render the cell.
   * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
   */
  @Override
  public Component getTableCellRendererComponent(
      JTable table, Object value,
      boolean isSelected, boolean hasFocus, int row, int column) {
    return getButton(value);
  }

  /**
   * Creates the button for the detection if it doesn't already exist.
   * 
   * @param value Detection.
   * @return Button for the detection.
   */
  private JButton getButton(Object value) {
    if (buttons.containsKey(value)) {
      return buttons.get(value);
    }
    String pageName = null;
    if (value instanceof String) {
      pageName = Page.normalizeTitle((String) value);
    } else if (value instanceof Page) {
      pageName = ((Page) value).getTitle();
    }
    if (pageName == null) {
      return null;
    }
    JButton button = new JButton(Utilities.getImageIcon(
        "gnome-system-run.png",
        EnumImageSize.SMALL));
    button.setBorderPainted(false);
    button.setContentAreaFilled(false);
    button.setActionCommand(pageName);
    button.setEnabled(true);
    button.addActionListener(EventHandler.create(
        ActionListener.class, this, "goTo", "actionCommand"));
    buttons.put(value, button);
    return button;
  }

  /**
   * @param location Location to go.
   */
  public void goTo(String location) {
    if (location != null) {
      Controller.runFullAnalysis(location, null, wiki);
    }
  }
}
