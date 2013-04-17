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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for checking language links existence.
 */
@SuppressWarnings("serial")
public class CheckLanguageLinkAction extends TextAction {

  private final EnumWikipedia fromWiki;
  private final EnumWikipedia toWiki;
  private final String title;
  private final Element element;
  private final JTextPane textPane;

  /**
   * @param fromWiki Wiki on which to check the language link existence.
   * @param toWiki Wiki to which the check has to be made.
   * @param title
   * @param element
   * @param textPane
   */
  public CheckLanguageLinkAction(
      EnumWikipedia fromWiki,
      EnumWikipedia toWiki,
      String title,
      Element element,
      JTextPane textPane) {
    super("CheckLanguageLink");
    this.fromWiki = fromWiki;
    this.toWiki = toWiki;
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
      StringBuilder message = new StringBuilder();
      List<String> values = new ArrayList<String>();

      // Check language link
      if ((fromWiki != null) && (toWiki != null)) {
        String languageLink = api.getLanguageLink(fromWiki, toWiki, title);
        if (message.length() > 0) {
          message.append("\n");
        }
        if (languageLink != null) {
          message.append(GT._(
              "The page {0} in \"{1}\" has a language link to \"{2}\": {3}.",
              new Object[] { title, fromWiki.toString(), toWiki.toString(), languageLink } ));
          String value = PageElementInternalLink.createInternalLink(languageLink, null);
          if (!values.contains(value)) {
            values.add(value);
          }
        } else {
          message.append(GT._(
              "The page {0} in \"{1}\" doesn''t have a language link to \"{2}\".",
              new Object[] { title, fromWiki.toString(), toWiki.toString() } ));
        }
      }

      // Check local page
      if (toWiki != null) {
        Page page = DataManager.getPage(toWiki, title, null, null, null);
        api.retrieveInfo(toWiki, Collections.singletonList(page));
        if (message.length() > 0) {
          message.append("\n");
        }
        if (Boolean.FALSE.equals(page.isExisting())) {
          message.append(GT._(
              "The page {0} doesn''t exist in \"{1}\".",
              new Object[] { title, toWiki.toString() } ));
        } else {
          message.append(GT._(
              "The page {0} exists in \"{1}\".",
              new Object[] { title, toWiki.toString() } ));
          String value = PageElementInternalLink.createInternalLink(title, null);
          if (!values.contains(value)) {
            values.add(value);
          }
        }
      }

      // Ask user
      if (values.isEmpty()) {
        Utilities.displayInformationMessage(
            textPane.getParent(), message.toString());
      } else {
        message.append("\n");
        message.append(GT._("What replacement do you want to use?"));
        Object result = Utilities.askForValue(
            textPane.getParent(), message.toString(),
            values.toArray(), values.get(0));
        if (result != null) {
          int startOffset = element.getStartOffset();
          int endOffset = element.getEndOffset();
          try {
            textPane.getDocument().remove(startOffset, endOffset - startOffset);
            textPane.getDocument().insertString(startOffset, result.toString(), element.getAttributes());
            textPane.setCaretPosition(startOffset);
            textPane.moveCaretPosition(startOffset + result.toString().length());
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
