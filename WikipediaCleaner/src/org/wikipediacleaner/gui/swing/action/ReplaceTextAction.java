/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;


/**
 * An action listener for replacing text.
 */
@SuppressWarnings("serial")
public class ReplaceTextAction extends TextAction {

  private final Page page;
  private final String newText;
  private final Element element;
  private final JTextPane textPane;

  public ReplaceTextAction(
      Page page,
      String newText,
      Element element,
      JTextPane textPane) {
    super("ReplaceText");
    this.page = page;
    this.newText = newText;
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
    replace(newText, localElement, localTextPane);
  }

  /**
   * Replace text. 
   * 
   * @param localNewText New text.
   * @param localElement Element.
   * @param localTextPane Text pane.
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

    // Text finalization
    if (page != null) {
      try {
        PageAnalysis analysis = page.getAnalysis(localNewText, false);
        List<PageElementFunction> functions = analysis.getFunctions();
        if ((functions != null) && (!functions.isEmpty())) {
          API api = APIFactory.getAPI();
          localNewText = api.parseText(page.getWikipedia(), page.getTitle(), localNewText, false);
        }
      } catch (APIException e) {
        // Nothing to do
      }
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
