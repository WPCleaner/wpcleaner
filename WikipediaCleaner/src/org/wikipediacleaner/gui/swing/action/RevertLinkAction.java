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
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;


/**
 * An action listener for reverting internal links ([[xx|yy]] gives [[yy|xx]]).
 */
public class RevertLinkAction implements ActionListener {

  private final String title;
  private final String text;
  private final Element element;
  private final JTextPane textPane;

  public RevertLinkAction(
      String title,
      String text,
      Element element,
      JTextPane textPane) {
    this.title = title;
    this.text = text;
    this.element = element;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((element == null) ||
        (textPane == null) ||
        (title == null) || (title.length() == 0) ||
        (text == null) || (text.length() == 0)) {
      return;
    }

    // Initialize
    int startOffset = element.getStartOffset();
    int endOffset = element.getEndOffset();
    String newText = "[[" + text + "|" + title + "]]";

    // Replace
    try {
      textPane.getDocument().remove(startOffset, endOffset - startOffset);
      textPane.getDocument().insertString(startOffset, newText, element.getAttributes());
      textPane.setCaretPosition(startOffset);
      textPane.moveCaretPosition(startOffset + newText.length());
    } catch (BadLocationException e1) {
      // Nothing to be done
    }
  }
}
