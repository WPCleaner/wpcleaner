/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractButton;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;


/**
 * An action listener for marking a link to a disambiguation page as normal.
 */
public class MarkLinkAction implements ActionListener {

  private final Element element;
  private final String newText;
  private final JTextPane textPane;
  private final AbstractButton checkBox;

  public MarkLinkAction(
      Element element,
      String newText,
      JTextPane textPane,
      AbstractButton checkBox) {
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

      // Initialize
      int startOffset = MWPaneFormatter.getUUIDStartOffset(textPane, element);
      int endOffset = MWPaneFormatter.getUUIDEndOffet(textPane, element);

      // Replace
      try {
        textPane.getDocument().remove(startOffset, endOffset - startOffset);
        textPane.getDocument().insertString(startOffset, newText, element.getAttributes());
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