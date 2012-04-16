/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.component;

import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.StyledDocument;


/**
 * Manage selection operations on MWPane.
 */
public class MWPaneSelectionManager {

  /**
   * MWPane.
   */
  private final MWPane textPane;

  /**
   * @param textPane MWPane.
   */
  MWPaneSelectionManager(MWPane textPane) {
    this.textPane = textPane;
  }

  /**
   * Select first occurrence of text. 
   */
  public void selectFirstOccurrence() {
    StyledDocument doc = textPane.getStyledDocument();
    int length = doc.getLength();
    int lastEnd = Integer.MAX_VALUE;
    for (int pos = 0; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(run);
        return;
      }
    }
    for (int pos = 0; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != null)) {
        select(run);
        return;
      }
    }
  }

  /**
   * Select previous occurrence of text. 
   */
  public void selectPreviousOccurrence() {
    StyledDocument doc = textPane.getStyledDocument();
    int lastStart = Integer.MIN_VALUE;
    for (int pos = textPane.getSelectionStart(); pos > 0; pos = lastStart) {
      Element run = doc.getCharacterElement(pos - 1);
      lastStart = run.getStartOffset();
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(run);
        return;
      }
    }
    selectLastOccurrence();
  }

  /**
   * Select next occurrence of text. 
   */
  public void selectNextOccurrence() {
    StyledDocument doc = textPane.getStyledDocument();
    int length = doc.getLength();
    int lastEnd = Integer.MAX_VALUE;
    for (int pos = textPane.getSelectionEnd() + 1; pos < length; pos = lastEnd) {
      Element run = doc.getCharacterElement(pos);
      lastEnd = run.getEndOffset();
      if (pos == lastEnd) {
        // offset + length beyond length of document, bail.
        break;
      }
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(run);
        return;
      }
    }
    selectFirstOccurrence();
  }

  /**
   * Select last occurrence of text. 
   */
  public void selectLastOccurrence() {
    StyledDocument doc = textPane.getStyledDocument();
    int lastStart = Integer.MIN_VALUE;
    for (int pos = doc.getLength(); pos > 0; pos = lastStart) {
      Element run = doc.getCharacterElement(pos - 1);
      lastStart = run.getStartOffset();
      MutableAttributeSet attr = (MutableAttributeSet) run.getAttributes();
      if ((attr != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_TYPE) != null) &&
          (attr.getAttribute(MWPaneFormatter.ATTRIBUTE_OCCURRENCE) != Boolean.FALSE)) {
        select(run);
        return;
      }
    }
  }

  /**
   * @param run Element to be selected.
   */
  private void select(Element run) {
    int startOffset = MWPaneFormatter.getUUIDStartOffset(textPane, run);
    int endOffset = MWPaneFormatter.getUUIDEndOffet(textPane, run);
    textPane.moveCaretPosition(startOffset);
    textPane.select(
        startOffset,
        endOffset);
  }
}
