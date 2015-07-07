/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;


/**
 * An Action that can apply several times the same replacement.
 */
public class MWPaneReplaceAllAction extends MWPaneAction {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -5712602406152066930L;

  private final String originalText;

  private final String newText;

  public MWPaneReplaceAllAction(String originalText, String newText) {
    super("MWPaneReplaceAllAction");
    this.originalText = originalText;
    this.newText = newText;
  }

  /**
   * @param e
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    MWPane textPane = getMWPane(e);
    if (textPane == null) {
      return;
    }
    StyledDocument doc = textPane.getStyledDocument();
    if (doc == null) {
      return;
    }
    int length = doc.getLength();
    int lastEnd = Integer.MAX_VALUE;
    for (int pos = 0; pos < length; pos = lastEnd) {
      try {
        Element run = doc.getCharacterElement(pos);
        lastEnd = run.getEndOffset();
        if (pos == lastEnd) {
          // offset + length beyond length of document, bail.
          break;
        }
        MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
        if ((attr != null) &&
            (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
            (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_INFO) != null)) {
          Object attrInfo = attr.getAttribute(MWPaneFormatter.ATTRIBUTE_INFO);
          if (attrInfo instanceof CheckErrorResult) {
            int startOffset = MWPaneFormatter.getUUIDStartOffset(textPane, run);
            int endOffset = MWPaneFormatter.getUUIDEndOffet(textPane, run);
            if (originalText.equals(textPane.getText(startOffset, endOffset - startOffset))) {
              boolean possible = false;
              CheckErrorResult info = (CheckErrorResult) attrInfo;
              List<Actionnable> actionnables = info.getPossibleActions();
              if (actionnables != null) {
                for (Actionnable actionnable : actionnables) {
                  possible |= actionnable.isPossibleReplacement(newText);
                }
              }
              if (possible) {
                doc.remove(startOffset, endOffset - startOffset);
                doc.insertString(startOffset, newText, attr);
                lastEnd = startOffset + newText.length();
              }
            }
          }
        }
      } catch (BadLocationException exception) {
        // Nothing to do
      }
    }
  }
}
