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
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for checking language links existence.
 */
@SuppressWarnings("serial")
public class CheckCategoryLinkAction extends TextAction {

  private final EnumWikipedia fromWikipedia;
  private final EnumWikipedia toWikipedia;
  private final String title;
  private final Element element;
  private final JTextPane textPane;

  public CheckCategoryLinkAction(
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
      Namespace categoryNamespace = Namespace.getNamespace(Namespace.CATEGORY, toWikipedia.getNamespaces());
      String categoryName = "Category";
      if (categoryNamespace != null) {
        if (!"Category".equals(categoryNamespace.getCanonicalTitle())) {
          categoryName = categoryNamespace.getCanonicalTitle();
        } else {
          for (String alias : categoryNamespace.getAliases()) {
            if (!"Category".equals(alias)) {
              categoryName = alias;
              break;
            }
          }
        }
      }

      API api = APIFactory.getAPI();
      Page category = DataManager.getPage(toWikipedia, "Category:" + title, null, null); 
      api.retrieveContents(toWikipedia, category, false);
      if (category.isExisting() == null) {
        Utilities.displayWarning(
            textPane.getParent(),
            GT._(
                "Unable to find if category {0} exists in the {1} Wikipedia.",
                new Object[] { title, toWikipedia.getCode() }));
        return;
      }
      if (Boolean.TRUE.equals(category.isExisting())) {
        int answer = Utilities.displayYesNoWarning(
            textPane.getParent(),
            GT._(
                "The category {0} exists in the {1} Wikipedia.\n" +
                "Do you want to replace the category by [[{2}]] ?",
                new Object[] { title, toWikipedia.getCode(), categoryName + ":" + title }));
        if (answer == JOptionPane.YES_OPTION) {
          int startOffset = element.getStartOffset();
          int endOffset = element.getEndOffset();
          try {
            String replace = "[[" + categoryName + ":" + title + "]]";
            textPane.getDocument().remove(startOffset, endOffset - startOffset);
            textPane.getDocument().insertString(startOffset, replace, element.getAttributes());
            textPane.setCaretPosition(startOffset);
            textPane.moveCaretPosition(startOffset + replace.length());
          } catch (BadLocationException e1) {
            // Nothing to be done
          }
        }
        return;
      }
      String languageLink = api.getLanguageLink(fromWikipedia, toWikipedia, "Category:" + title);
      if (languageLink == null) {
        Utilities.displayInformationMessage(
            textPane.getParent(),
            GT._(
                "The category {0} in the {1} Wikipedia doesn''t have a language link to the {2} Wikipedia.\n" +
                "It doesn''t exist either in the {2} Wikipedia.",
                new Object[] { title, fromWikipedia.getCode(), toWikipedia.getCode() } ));
        return;
      }
      int answer = Utilities.displayYesNoWarning(
          textPane.getParent(),
          GT._(
              "The category {0} doesn''t exist in the {2} Wikipedia.\n" +
              "In the {1} Wikipedia, it has a language link to the {2} Wikipedia: {3}.\n" +
              "Do you want to replace the category by [[{3}]] ?",
              new Object[] { title, fromWikipedia.getCode(), toWikipedia.getCode(), languageLink } ));
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
    } catch (APIException e1) {
      //
    }
  }
}
