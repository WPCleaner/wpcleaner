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

import javax.swing.JCheckBox;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;


/**
 * An action listener for marking a link to a disambiguation page as normal.
 */
public class MarkLinkAction implements ActionListener {

  private final Element element;
  private final String newText;
  private final JTextPane textPane;
  private final JCheckBox checkBox;

  public MarkLinkAction(
      Element element,
      String newText,
      JTextPane textPane,
      JCheckBox checkBox) {
    this.newText = newText;
    this.element = element;
    this.textPane = textPane;
    this.checkBox = checkBox;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((element != null) &&
        (textPane != null) &&
        (newText != null)) {

      // Initialize
      int startOffset = element.getStartOffset();
      int endOffset = element.getEndOffset();

      // Replace
      try {
        textPane.getDocument().remove(startOffset, endOffset - startOffset);
        textPane.getDocument().insertString(startOffset, newText, element.getAttributes());
      } catch (BadLocationException e1) {
        // Nothing to be done
      }

      // Checkbox
      if ((checkBox != null) && (checkBox.isEnabled())) {
        checkBox.setSelected(true);
      }
    }
  }
}