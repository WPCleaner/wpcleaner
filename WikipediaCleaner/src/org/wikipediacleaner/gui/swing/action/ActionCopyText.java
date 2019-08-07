/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for copying text.
 */
public class ActionCopyText implements ActionListener, ClipboardOwner {

  /**
   * Create a menu item for copying text.
   * 
   * @param textPane Text pane.
   * @return Menu item.
   */
  public static JMenuItem createMenuItem(MWPane textPane) {
    JMenuItem menuItem = Utilities.createJMenuItem(
        GT._T("Copy selected text"), true, "gnome-edit-copy.png", EnumImageSize.SMALL);
    menuItem.addActionListener(new ActionCopyText(textPane));
    return menuItem;
  }

  /**
   * Text pane.
   */
  private final MWPane textPane;

  /**
   * @param textPane Text pane.
   */
  private ActionCopyText(MWPane textPane) {
    this.textPane = textPane;
  }

  /**
   * Copy text.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (textPane == null)) {
      return;
    }

    String selectedText = textPane.getSelectedText();
    if (selectedText == null) {
      selectedText = "";
    }
    StringSelection selection = new StringSelection(selectedText);
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    clipboard.setContents(selection, this);
  }

  /**
   * Notifies this object that it is no longer the clipboard owner.
   * This method will be called when another application or another
   * object within this application asserts ownership of the clipboard.
   *
   * @param clipboard the clipboard that is no longer owned
   * @param contents the contents which this owner had placed on the clipboard
   * @see java.awt.datatransfer.ClipboardOwner#lostOwnership(java.awt.datatransfer.Clipboard, java.awt.datatransfer.Transferable)
   */
  @Override
  public void lostOwnership(Clipboard clipboard, Transferable contents) {
    // Do nothing
  }
}
