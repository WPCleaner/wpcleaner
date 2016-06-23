/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.Collection;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.TextProvider;


/**
 * An action listener for replacing text by an internal link.
 */
@SuppressWarnings("serial")
public class AddInternalLinkAction extends TextAction {

  private final String article;
  private final String prefix;
  private final String suffix;
  private final TextProvider textProvider;
  private final String question;
  private final String[] possibleValues;
  private final boolean onlyList;
  private final String defaultValue;
  private final StringChecker checker;
  private final Element element;
  private final JTextPane textPane;

  public AddInternalLinkAction(
      String article,
      String prefix,
      String suffix,
      TextProvider textProvider,
      String question,
      String defaultValue,
      StringChecker checker,
      Element element,
      JTextPane textPane) {
    this(
        article, prefix, suffix, textProvider, question,
        null, false, defaultValue,
        checker, element, textPane);
  }

  public AddInternalLinkAction(
      String article,
      String prefix,
      String suffix,
      TextProvider textProvider,
      String question,
      String[] possibleValues,
      boolean onlyList,
      String defaultValue,
      StringChecker checker,
      Element element,
      JTextPane textPane) {
    super("AddInternalLink");
    this.article = article;
    this.prefix = prefix;
    this.suffix = suffix;
    this.textProvider = textProvider;
    this.question = question;
    this.possibleValues = possibleValues;
    this.onlyList = onlyList;
    this.defaultValue = defaultValue;
    this.checker = checker;
    this.element = element;
    this.textPane = textPane;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
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
    String value = null;
    String[] tmpPossibleValues = null;
    if ((possibleValues != null) && (possibleValues.length > 0)) {
      tmpPossibleValues = possibleValues;
    }
    if (textProvider != null) {
      Collection<String> texts = textProvider.getTexts();
      if ((texts != null) && (texts.size() > 0)) {
        value = texts.iterator().next();
        if (tmpPossibleValues == null) {
          tmpPossibleValues = new String[texts.size()];
          tmpPossibleValues = texts.toArray(tmpPossibleValues);
        }
      }
    }
    if (value == null) {
      value = defaultValue;
    }
    if (tmpPossibleValues != null) {
      value = Utilities.askForValue(
          (localTextPane != null) ? localTextPane.getParent() : null,
          question,
          tmpPossibleValues, onlyList, value, checker);
    } else {
      value = Utilities.askForValue(
          (localTextPane != null) ? localTextPane.getParent() : null,
          question, value, checker);
    }
    if ((value != null) && (!value.isEmpty())) {
      StringBuilder newText = new StringBuilder();
      if (prefix != null) {
        newText.append(prefix);
      }
      newText.append(PageElementInternalLink.createInternalLink(article, value));
      if (suffix != null) {
        newText.append(suffix);
      }
      replace(newText.toString(), localElement, localTextPane);
    }
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
    int startOffset = MWPaneFormatter.getUUIDStartOffset(localTextPane, localElement);
    int endOffset = MWPaneFormatter.getUUIDEndOffet(localTextPane, localElement);

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
