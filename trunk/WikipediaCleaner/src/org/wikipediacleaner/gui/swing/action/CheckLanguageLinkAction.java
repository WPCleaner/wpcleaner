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

import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for checking language links existence.
 */
@SuppressWarnings("serial")
public class CheckLanguageLinkAction extends TextAction {

  private final EnumWikipedia fromWikipedia;
  private final EnumWikipedia toWikipedia;
  private final String title;
  private final Element element;
  private final JTextPane textPane;

  public CheckLanguageLinkAction(
      EnumWikipedia fromWikipedia,
      EnumWikipedia toWikipedia,
      String title,
      Element element,
      JTextPane textPane) {
    super("CheckLanguageLink");
    this.fromWikipedia = fromWikipedia;
    this.toWikipedia = toWikipedia;
    this.title = title;
    this.element = element;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    try {
      API api = APIFactory.getAPI();
      String languageLink = api.getLanguageLink(fromWikipedia, toWikipedia, title);
      if (languageLink == null) {
        Utilities.displayInformationMessage(
            textPane.getParent(),
            GT._("The page {0} in \"{1}\" doesn''t have a language link to \"{2}\".",
                new Object[] { title, fromWikipedia.toString(), toWikipedia.toString() } ));
      } else {
        int answer = Utilities.displayYesNoWarning(
            textPane.getParent(),
            GT._(
                "The page {0} in \"{1}\" has a language link to \"{2}\": {3}.\n" +
                "Do you want to replace the link by [[{3}]] ?",
                new Object[] { title, fromWikipedia.toString(), toWikipedia.toString(), languageLink } ));
        if (answer == JOptionPane.YES_OPTION) {
          int startOffset = element.getStartOffset();
          int endOffset = element.getEndOffset();
          try {
            textPane.getDocument().remove(startOffset, endOffset - startOffset);
            textPane.getDocument().insertString(startOffset, "[[" + languageLink + "]]", element.getAttributes());
            textPane.setCaretPosition(startOffset);
            textPane.moveCaretPosition(startOffset + languageLink.length() + 4);
          } catch (BadLocationException e1) {
            // Nothing to be done
          }
        }
      }
    } catch (APIException e1) {
      //
    }
  }
}
