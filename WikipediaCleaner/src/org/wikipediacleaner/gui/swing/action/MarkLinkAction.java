/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;


/**
 * An action listener for marking a link to a disambiguation page as normal.
 */
public class MarkLinkAction implements ActionListener {

  private final Page page;
  private final Element element;
  private final String newText;
  private final JTextPane textPane;
  private final AbstractButton checkBox;

  public MarkLinkAction(
      Page page,
      Element element,
      String newText,
      JTextPane textPane,
      AbstractButton checkBox) {
    this.page = page;
    this.newText = newText;
    this.element = element;
    this.textPane = textPane;
    this.checkBox = checkBox;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((element != null) &&
        (textPane != null) &&
        (newText != null)) {

      // Text finalization
      String localNewText = newText;
      if (page != null) {
        try {
          PageAnalysis analysis = page.getAnalysis(localNewText, false);
          List<PageElementFunction> functions = analysis.getFunctions();
          boolean parseNeeded = false;
          if (functions != null) {
            for (PageElementFunction function : functions) {
              if (function.getMagicWord() == null) {
                parseNeeded = true;
              } else if (!function.getMagicWord().isFunctionNotPSTMagicWord()) {
                parseNeeded = true;
              }
            }
          }
          if (parseNeeded) {
            API api = APIFactory.getAPI();
            localNewText = api.parseText(page.getWikipedia(), page.getTitle(), localNewText, false);
          }
        } catch (APIException ex) {
          // Nothing to do
        }
      }

      // Initialize
      int startOffset = MWPaneFormatter.getUUIDStartOffset(textPane, element);
      int endOffset = MWPaneFormatter.getUUIDEndOffet(textPane, element);

      // Replace
      try {
        textPane.getDocument().remove(startOffset, endOffset - startOffset);
        textPane.getDocument().insertString(startOffset, localNewText, element.getAttributes());
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