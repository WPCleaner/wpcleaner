/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;


/**
 * An action listener for reverting internal links ([[xx|yy]] -> [[yy|xx]]).
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
