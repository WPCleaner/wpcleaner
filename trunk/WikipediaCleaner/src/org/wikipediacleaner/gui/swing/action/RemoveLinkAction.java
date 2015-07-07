/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JTextPane;


/**
 * An action listener for removing internal links.
 */
public class RemoveLinkAction implements ActionListener {

  private final String text;
  private final int startOffset;
  private final int endOffset;
  private final JTextPane textPane;

  public RemoveLinkAction(
      String text,
      JTextPane textPane,
      int startOffset, int endOffset) {
    this.text = text;
    this.startOffset = startOffset;
    this.endOffset = endOffset;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((textPane != null) &&
        (text != null) &&
        (text.length() > 0)) {
      try {
        textPane.setCaretPosition(startOffset);
        textPane.moveCaretPosition(endOffset);
        textPane.replaceSelection(text);
      } catch (IllegalArgumentException ex) {
        //
      }
    }
  }
}
