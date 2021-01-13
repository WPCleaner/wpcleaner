/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.HashMap;

import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Cell renderer and editor for copying text.
 */
public class CopyCellRenderer extends AbstractCellEditor implements
    TableCellRenderer, TableCellEditor {

  /** Serialization */
  private static final long serialVersionUID = 4734178589250415359L;

  /** Maps of all the buttons. */
  private HashMap<Object, JButton> buttons;

  /** Column number to copy. */
  private final int copyColumn;

  /**
   * @param copyColumn Column number to copy.
   */
  public CopyCellRenderer(int copyColumn) {
    buttons = new HashMap<>();
    this.copyColumn = copyColumn;
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
  public Component getTableCellEditorComponent(JTable table, Object value,
      boolean isSelected, int row, int column) {
    return getButton(table, row);
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
  public Component getTableCellRendererComponent(JTable table, Object value,
      boolean isSelected, boolean hasFocus, int row, int column) {
    return getButton(table, row);
  }

  /**
   * Creates the button for the detection if it doesn't already exist.
   * 
   * @param table Table.
   * @param row Row.
   * @return Button for the row.
   */
  private JButton getButton(JTable table, int row) {
    Integer rowNum = Integer.valueOf(row);
    if (buttons.containsKey(rowNum)) {
      return buttons.get(rowNum);
    }
    JButton button = new JButton(Utilities.getImageIcon(
        "gnome-edit-copy.png", EnumImageSize.SMALL));
    button.setBorderPainted(false);
    button.setContentAreaFilled(false);
    Object value = table.getValueAt(row, copyColumn);
    button.setActionCommand(value != null ? value.toString() : "");
    button.setEnabled(true);
    button.addActionListener(EventHandler.create(
        ActionListener.class, this, "copyCell", "actionCommand"));
    buttons.put(rowNum, button);
    return button;
  }

  /**
   * @param value Value to copy.
   */
  public void copyCell(String value) {
    Toolkit toolkit = Toolkit.getDefaultToolkit();
    Clipboard clipboard = toolkit.getSystemClipboard();
    StringSelection strSel = new StringSelection(value);
    clipboard.setContents(strSel, null);
  }
}
