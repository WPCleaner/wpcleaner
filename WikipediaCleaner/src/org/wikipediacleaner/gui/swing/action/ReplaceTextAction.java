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

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;


/**
 * An action listener for replacing text.
 */
@SuppressWarnings("serial")
public class ReplaceTextAction extends TextAction {

  private final String newText;
  private final Element element;
  private final JTextPane textPane;

  public ReplaceTextAction(
      String newText,
      Element element,
      JTextPane textPane) {
    super("ReplaceLink");
    this.newText = newText;
    this.element = element;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    JTextPane localTextPane = textPane;
    if (localTextPane == null) {
      JTextComponent textComponent = getTextComponent(e);
      if (textComponent instanceof JTextPane) {
        localTextPane = (JTextPane) textComponent;
      }
    }
    Element localElement = element;
    if ((localTextPane != null) && (localElement == null)) {
      localElement = localTextPane.getStyledDocument().getCharacterElement(
          localTextPane.getSelectionStart());
    }
    replace(newText, localElement, localTextPane);
  }

  /**
   * Replace text. 
   */
  private void replace(
      String localNewText,
      Element localElement,
      JTextPane localTextPane) {
    if ((localElement == null) ||
        (localTextPane == null) ||
        (localNewText == null)) {
      return;
    }

    // Initialize
    int startOffset = localElement.getStartOffset();
    int endOffset = localElement.getEndOffset();
    Object uuid = localElement.getAttributes().getAttribute(MWPaneFormatter.ATTRIBUTE_UUID);
    if (uuid != null) {
      boolean finished;
      do {
        finished = true;
        if (startOffset > 0) {
          Element tmpElement = localTextPane.getStyledDocument().getCharacterElement(startOffset - 1); 
          if ((tmpElement != null) && (tmpElement.getAttributes() != null)) {
            if ((localElement != tmpElement) &&
                (uuid.equals(tmpElement.getAttributes().getAttribute(MWPaneFormatter.ATTRIBUTE_UUID)))) {
              startOffset = tmpElement.getStartOffset();
              finished = false;
            }
          }
        }
      } while (!finished);
      do {
        finished = true;
        if (endOffset < localTextPane.getText().length()) {
          Element tmpElement = localTextPane.getStyledDocument().getCharacterElement(endOffset);
          if ((tmpElement != null) && (tmpElement.getAttributes() != null)) {
            if ((localElement != tmpElement) &&
                (uuid.equals(tmpElement.getAttributes().getAttribute(MWPaneFormatter.ATTRIBUTE_UUID)))) {
              endOffset = tmpElement.getEndOffset();
              finished = false;
            }
          }
        }
      } while (!finished);
    }

    // Replace
    try {
      localTextPane.getDocument().remove(startOffset, endOffset - startOffset);
      localTextPane.getDocument().insertString(startOffset, localNewText, localElement.getAttributes());
      localTextPane.setCaretPosition(startOffset);
      localTextPane.moveCaretPosition(startOffset + localNewText.length());
    } catch (BadLocationException e1) {
      // Nothing to be done
    }
  }
}
