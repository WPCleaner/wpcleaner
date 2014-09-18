/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.checkwiki;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.HashMap;

import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Cell renderer and editor for a Check Wiki detection.
 */
public class DetectionRenderer extends AbstractCellEditor implements
    TableCellRenderer, TableCellEditor {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 6604607833138200300L;

  /**
   * Maps of all the buttons.
   */
  private HashMap<Object, JButton> buttons;

  /**
   * Text pane where the text is.
   */
  private final JTextComponent textPane;

  public DetectionRenderer(JTextComponent textPane) {
    buttons = new HashMap<Object, JButton>();
    this.textPane = textPane;
  }

  /**
   * @return
   * @see javax.swing.CellEditor#getCellEditorValue()
   */
  public Object getCellEditorValue() {
    return null;
  }

  /**
   * @param table
   * @param value
   * @param isSelected
   * @param row
   * @param column
   * @return
   * @see javax.swing.table.TableCellEditor#getTableCellEditorComponent(javax.swing.JTable, java.lang.Object, boolean, int, int)
   */
  public Component getTableCellEditorComponent(JTable table, Object value,
      boolean isSelected, int row, int column) {
    return getButton(value);
  }

  /**
   * @param table
   * @param value
   * @param isSelected
   * @param hasFocus
   * @param row
   * @param column
   * @return
   * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent(JTable table, Object value,
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
    if ((value == null) || !(value instanceof CheckWikiDetection)) {
      return null;
    }
    CheckWikiDetection detection = (CheckWikiDetection) value;
    JButton button = new JButton(Utilities.getImageIcon(
        "gnome-edit-find.png", EnumImageSize.NORMAL));
    button.setBorderPainted(false);
    button.setContentAreaFilled(false);
    button.setActionCommand(Integer.toString(detection.getLocation()));
    button.setEnabled(textPane != null);
    button.addActionListener(EventHandler.create(
        ActionListener.class, this, "goToDetection", "actionCommand"));
    buttons.put(value, button);
    return button;
  }

  /**
   * @param location Location to go.
   */
  public void goToDetection(String location) {
    if (textPane != null) {
      try {
        int locationValue = Integer.valueOf(location);
        if ((locationValue >= 0) && (locationValue < textPane.getText().length())) {
          textPane.setCaretPosition(locationValue);
          textPane.moveCaretPosition(locationValue);
        }
      } catch (NumberFormatException e) {
        //
      }
    }
  }
}
