/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.Collections;

import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementCategory;
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
  private final String order;
  private final Element element;
  private final JTextPane textPane;

  public CheckCategoryLinkAction(
      EnumWikipedia fromWikipedia,
      EnumWikipedia toWikipedia,
      String title,
      String order,
      Element element,
      JTextPane textPane) {
    super("CheckLanguageLink");
    this.fromWikipedia = fromWikipedia;
    this.toWikipedia = toWikipedia;
    this.title = title;
    this.order = order;
    this.element = element;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    try {
      Namespace categoryNamespace = toWikipedia.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
      String categoryName = PageElementCategory.DEFAULT_NAME;
      if (categoryNamespace != null) {
        if (!PageElementCategory.DEFAULT_NAME.equals(
            categoryNamespace.getCanonicalTitle())) {
          categoryName = categoryNamespace.getCanonicalTitle();
        } else {
          for (String alias : categoryNamespace.getAliases()) {
            if (!PageElementCategory.DEFAULT_NAME.equals(alias)) {
              categoryName = alias;
              break;
            }
          }
        }
      }

      API api = APIFactory.getAPI();
      Page category = DataManager.getPage(toWikipedia, "Category:" + title, null, null, null); 
      api.retrieveContents(toWikipedia, Collections.singletonList(category), false, false);
      if (category.isExisting() == null) {
        Utilities.displayWarning(
            textPane.getParent(),
            GT._T(
                "Unable to find if category {0} exists in \"{1}\".",
                new Object[] { title, toWikipedia.toString() }));
        return;
      }
      if (Boolean.TRUE.equals(category.isExisting())) {
        String replace = categoryName + ":" + title + ((order != null) ? "|" + order : "");
        int answer = Utilities.displayYesNoWarning(
            textPane.getParent(),
            GT._T(
                "The category {0} exists in \"{1}\".\n" +
                "Do you want to replace the category by [[{2}]] ?",
                new Object[] { title, toWikipedia.toString(), replace }));
        if (answer == JOptionPane.YES_OPTION) {
          int startOffset = element.getStartOffset();
          int endOffset = element.getEndOffset();
          try {
            textPane.getDocument().remove(startOffset, endOffset - startOffset);
            textPane.getDocument().insertString(startOffset, "[[" + replace + "]]", element.getAttributes());
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
            GT._T(
                "The category {0} in the {1} Wikipedia doesn''t have a language link to the {2} Wikipedia.\n" +
                "It doesn''t exist either in the {2} Wikipedia.",
                new Object[] { title, fromWikipedia.getSettings().getCode(), toWikipedia.getSettings().getCode() } ));
        return;
      }
      String replace = languageLink + ((order != null) ? "|" + order : "");
      int answer = Utilities.displayYesNoWarning(
          textPane.getParent(),
          GT._T(
              "The category {0} doesn''t exist in the {2} Wikipedia.\n" +
              "In the {1} Wikipedia, it has a language link to the {2} Wikipedia: {3}.\n" +
              "Do you want to replace the category by [[{3}]] ?",
              new Object[] { title, fromWikipedia.getSettings().getCode(), toWikipedia.getSettings().getCode(), replace } ));
      if (answer == JOptionPane.YES_OPTION) {
        int startOffset = element.getStartOffset();
        int endOffset = element.getEndOffset();
        try {
          textPane.getDocument().remove(startOffset, endOffset - startOffset);
          textPane.getDocument().insertString(startOffset, "[[" + replace + "]]", element.getAttributes());
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
