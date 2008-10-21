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
import javax.swing.text.Element;


/**
 * An action listener for removing internal links.
 */
public class RemoveLinkAction implements ActionListener {

  private final String text;
  private final Element element;
  private final JTextPane textPane;

  public RemoveLinkAction(
      String text,
      Element element,
      JTextPane textPane) {
    this.text = text;
    this.element = element;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((element != null) &&
        (textPane != null) &&
        (text != null) &&
        (text.length() > 0)) {
      textPane.setCaretPosition(element.getStartOffset());
      textPane.moveCaretPosition(element.getEndOffset());
      textPane.replaceSelection(text);
    }
  }
}
