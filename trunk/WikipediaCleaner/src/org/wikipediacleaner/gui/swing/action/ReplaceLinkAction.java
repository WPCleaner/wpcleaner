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
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.component.MenuCreator;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * An action listener for replacing internal links.
 */
@SuppressWarnings("serial")
public class ReplaceLinkAction extends TextAction {

  private final String oldTitle;
  private final String newTitle;
  private final String text;
  private final Element element;
  private final JTextPane textPane;
  private final boolean fullReplacement;

  public ReplaceLinkAction(boolean fullReplacement) {
    this(null, null, null, null, null, fullReplacement);
  }

  public ReplaceLinkAction(
      String oldTitle,
      String newTitle,
      String text,
      Element element,
      JTextPane textPane,
      boolean fullReplacement) {
    super("ReplaceLink");
    this.oldTitle = oldTitle;
    this.newTitle = newTitle;
    this.text = text;
    this.element = element;
    this.textPane = textPane;
    this.fullReplacement = fullReplacement;
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
    String localText = text;
    if ((localElement != null) && (localText == null)) {
      Object attrText = localElement.getAttributes().getAttribute(MWPaneFormatter.ATTRIBUTE_TEXT);
      if (attrText instanceof String) {
        localText = (String) attrText;
      }
    }
    String localOldTitle = oldTitle;
    if ((localElement != null) && (localOldTitle == null)) {
      Object attrPage = localElement.getAttributes().getAttribute(MWPaneFormatter.ATTRIBUTE_PAGE);
      if (attrPage instanceof Page) {
        localOldTitle = ((Page) attrPage).getTitle();
      }
    }
    String localNewTitle = newTitle;
    if ((localOldTitle != null) && (localNewTitle == null)) {
      localNewTitle = MenuCreator.getLastReplacement(localOldTitle);
    }
    if (fullReplacement) {
      fullyReplace(localOldTitle, localNewTitle, localText, localElement, localTextPane);
    } else {
      replace(localOldTitle, localNewTitle, localText, localElement, localTextPane);
    }
  }

  /**
   * Replace link and displayed text. 
   */
  private void fullyReplace(
      String localOldTitle,
      String localNewTitle,
      String localText,
      Element localElement,
      JTextPane localTextPane) {
    if ((localElement != null) &&
        (localTextPane != null) &&
        (localNewTitle != null) &&
        (localNewTitle.length() > 0)) {
      localTextPane.setCaretPosition(MWPaneFormatter.getUUIDStartOffset(localTextPane, localElement));
      localTextPane.moveCaretPosition(MWPaneFormatter.getUUIDEndOffet(localTextPane, localElement));
      String newText = null;
      if ((localText != null) &&
          (localText.length() > 0) &&
          (localNewTitle.length() > 0) &&
          (localText.charAt(0) != localNewTitle.charAt(0)) &&
          (Character.toUpperCase(localText.charAt(0)) == Character.toUpperCase(localNewTitle.charAt(0)))) {
        newText = "[[" + localText.charAt(0) + localNewTitle.substring(1) + "]]";
      } else {
        newText = "[[" + localNewTitle + "]]";
      }
      localTextPane.replaceSelection(newText);
      MenuCreator.addLastReplacement(localOldTitle, localNewTitle);
    }
  }

  /**
   * Replace link (but keep displayed text). 
   */
  private void replace(
      String localOldTitle,
      String localNewTitle,
      String localText,
      Element localElement,
      JTextPane localTextPane) {
    if ((localElement == null) ||
        (localTextPane == null) ||
        (localNewTitle == null) || (localNewTitle.length() == 0) ||
        (localText == null) || (localText.length() == 0)) {
      return;
    }

    // Initialize
    int startOffset = MWPaneFormatter.getUUIDStartOffset(localTextPane, localElement);
    int endOffset = MWPaneFormatter.getUUIDEndOffet(localTextPane, localElement);
    String newText = null;
    int offsetBefore = 0;
    int offsetAfter = 0;
    boolean firstCharEqual = (Character.toUpperCase(localNewTitle.charAt(0)) == Character.toUpperCase(localText.charAt(0)));
    String title2 = localNewTitle.substring(1);
    String text2 = localText.substring(1);
    String originalNewTitle = localNewTitle;
    if (firstCharEqual && (localNewTitle.charAt(0) != localText.charAt(0))) {
      localNewTitle = "" + localText.charAt(0) + title2;
    }

    // Check for equality between title and text
    if (firstCharEqual && title2.equals(text2)) {
      newText = "[[" + localText + "]]";
    }

    // Check for possible extension of the title to the right
    if ((newText == null) && firstCharEqual && title2.startsWith(text2)) {
      try {
        String textAfter = localNewTitle.substring(localText.length());
        if (textAfter.equals(localTextPane.getText(localElement.getEndOffset(), textAfter.length()))) {
          newText = "[[" + localText.charAt(0) + title2 + "]]";
          offsetAfter = textAfter.length();
        }
      } catch (BadLocationException e2) {
        // Nothing to be done
      }
    }

    // Check for possible extension of the title to the left (and possibly to the right)
    int position;
    if ((newText == null) && ((position = localNewTitle.indexOf(localText)) > 0)) {
      try {
        String textBefore = localNewTitle.substring(0, position);
        String currentText = localTextPane.getText(startOffset - position, position);
        if ((Character.toUpperCase(textBefore.charAt(0)) == Character.toUpperCase(currentText.charAt(0))) &&
            textBefore.substring(1).equals(currentText.substring(1))) {
          if (position + localText.length() < localNewTitle.length()) {
            // Check right
            String textAfter = localNewTitle.substring(position + localText.length());
            String currentText2 = localTextPane.getText(endOffset, localNewTitle.length() - position - localText.length());
            if (textAfter.equals(currentText2)) {
              newText = "[[" + currentText.charAt(0) + localNewTitle.substring(1) + "]]";
              offsetBefore = position;
              offsetAfter = currentText2.length();
            }
          } else {
            newText = "[[" + currentText.charAt(0) + localNewTitle.substring(1) + "]]";
            offsetBefore = position;
          }
        }
      } catch (BadLocationException e2) {
        // Nothing to be done
      }
    }

    // Check with parenthesis removed
    Configuration config = Configuration.getConfiguration();
    if (config.getBoolean(
        null,
        ConfigurationValueBoolean.SHORT_NOTATION)) {
      String titleRTrimmed = localNewTitle.replaceAll("\\s*\\(.*\\)\\s*$", "");
      if ((newText == null) && firstCharEqual && titleRTrimmed.substring(1).equals(text2)) {
        newText = "[[" + localText.charAt(0) + title2 + "|]]";
      }
    }

    // Default replacement
    if (newText == null) {
      newText = "[[" + localNewTitle + "|" + localText + "]]";
    }

    // Replace
    try {
      String textAttr = localText;
      if ((offsetBefore != 0) || (offsetAfter != 0)) {
        textAttr =
            localTextPane.getText(startOffset - offsetBefore, offsetBefore) +
            localText +
            localTextPane.getText(endOffset, offsetAfter);
      }
      localTextPane.getDocument().remove(startOffset - offsetBefore, endOffset + offsetAfter - startOffset + offsetBefore);
      localTextPane.getDocument().insertString(startOffset - offsetBefore, newText, localElement.getAttributes());
      if ((offsetBefore != 0) || (offsetAfter != 0)) {
        SimpleAttributeSet attr = new SimpleAttributeSet();
        attr.addAttribute(MWPaneFormatter.ATTRIBUTE_TEXT, textAttr);
        localTextPane.getStyledDocument().setCharacterAttributes(
            startOffset - offsetBefore, newText.length(), attr, false);
      }
      localTextPane.setCaretPosition(startOffset - offsetBefore);
      localTextPane.moveCaretPosition(startOffset - offsetBefore + newText.length());
      MenuCreator.addLastReplacement(localOldTitle, originalNewTitle);
    } catch (BadLocationException e1) {
      // Nothing to be done
    }
  }
}
