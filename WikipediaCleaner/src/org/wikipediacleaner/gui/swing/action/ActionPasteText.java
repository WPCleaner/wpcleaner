/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.JMenuItem;
import javax.swing.text.BadLocationException;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for pasting text.
 */
public class ActionPasteText implements ActionListener {

  /**
   * Create a menu item for pasting text.
   * 
   * @param textPane Text pane.
   * @return Menu item.
   */
  public static JMenuItem createMenuItem(MWPane textPane) {
    String clipboard = getClipboardText();
    if ((clipboard == null) || (clipboard.isEmpty())) {
      return null;
    }
    JMenuItem menuItem = Utilities.createJMenuItem(
        GT._T("Paste"), true, "gnome-edit-paste.png", EnumImageSize.SMALL);
    menuItem.addActionListener(new ActionPasteText(textPane));
    return menuItem;
  }

  /**
   * Text pane.
   */
  private final MWPane textPane;

  /**
   * @param textPane Text pane.
   */
  private ActionPasteText(MWPane textPane) {
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

    String result = getClipboardText();
    if (result != null) {
      try {
        int beginSelection = textPane.getSelectionStart();
        int endSelection = textPane.getSelectionEnd();
        textPane.getDocument().insertString(endSelection, result, null);
        textPane.select(endSelection, endSelection + result.length());
        if (endSelection > beginSelection) {
          textPane.getDocument().remove(beginSelection, endSelection - beginSelection);
        }
      } catch (BadLocationException ex) {
        // Do nothing
      }
    }
  }

  /**
   * Retrieve text from the clip board.
   * 
   * @return Text from the clip board.
   */
  private static String getClipboardText() {
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    Transferable contents = clipboard.getContents(null);
    if ((contents == null) || !contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      return null;
    }
    try {
      return (String) contents.getTransferData(DataFlavor.stringFlavor);
    } catch (UnsupportedFlavorException|IOException ex) {
      // Do nothing
    }
    return null;
  }
}
