/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

import java.util.UUID;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.utils.ConfigurationValueStyle;


/**
 * A Check Wiki formatter for MediaWikiPane.
 */
public class MWPaneCheckWikiFormatter extends MWPaneFormatter {

  /**
   * Construct a Check Wiki formatter.
   * 
   * @param algorithm Algorithm.
   */
  public MWPaneCheckWikiFormatter(
      CheckErrorAlgorithm algorithm) {
    this.algorithm = algorithm;
  }

  /**
   * Format text in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  @Override
  public void format(MWPane pane, PageAnalysis pageAnalysis) {
    // Clean formatting
    cleanFormat(pane);

    // Reset caret informations
    resetCaretPosition();

    // Format comments
    defaultFormatElements(pane, pageAnalysis);

    // Format Check Wiki errors
    formatCheckWikiErrors(pane, pageAnalysis);

    // Move caret
    moveCaret(pane);
  }

  /**
   * Format Check Wiki errors in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatCheckWikiErrors(
      MWPane pane, PageAnalysis pageAnalysis) {
    if ((pane == null) || (pageAnalysis == null) || (algorithm == null)) {
      return;
    }
    CheckErrorPage errorPage = CheckError.analyzeError(algorithm, pageAnalysis);
    if ((errorPage != null) && (errorPage.getResults() != null)) {
      for (CheckErrorResult error : errorPage.getResults()) {
        formatCheckWikiError(pane, error);
      }
    }
  }

  /**
   * Format a Check Wiki error in a MediaWikiPane.
   * 
   * @param pane MediaWikiPane to be formatted.
   * @param error Check Wiki error to be formatted.
   */
  private void formatCheckWikiError(
      MWPane pane, CheckErrorResult error) {

    // Basic verifications
    if ((pane == null) || (error == null)) {
      return;
    }

    // Format error
    ConfigurationValueStyle styleConfig = ConfigurationValueStyle.CHECK_WIKI_ERROR;
    if (error.getErrorLevel() == CheckErrorResult.ErrorLevel.CORRECT) {
      styleConfig = ConfigurationValueStyle.CHECK_WIKI_OK;
    } else if (error.getErrorLevel() == CheckErrorResult.ErrorLevel.WARNING) {
      styleConfig = ConfigurationValueStyle.CHECK_WIKI_WARNING;
    }
    StyledDocument doc = pane.getStyledDocument();
    if (doc == null) {
      return;
    }
    doc.setCharacterAttributes(
        error.getStartPosition(),
        error.getLength(),
        pane.getStyle(styleConfig.getName()),
        true);
    SimpleAttributeSet attributes = new SimpleAttributeSet();
    attributes.addAttribute(MWPaneFormatter.ATTRIBUTE_INFO, error);
    attributes.addAttribute(MWPaneFormatter.ATTRIBUTE_UUID, UUID.randomUUID());
    doc.setCharacterAttributes(
        error.getStartPosition(),
        error.getLength(),
        attributes, false);

    // Manage position
    if (error.getErrorLevel() == CheckErrorResult.ErrorLevel.CORRECT) {
      if (error.getStartPosition() < thirdStartPosition) {
        thirdStartPosition = error.getStartPosition();
        thirdEndPosition = error.getEndPosition();
      }
    } else if (error.getErrorLevel() == CheckErrorResult.ErrorLevel.WARNING) {
      if (error.getStartPosition() < secondStartPosition) {
        secondStartPosition = error.getStartPosition();
        secondEndPosition = error.getEndPosition();
      }
    } else {
      if (error.getStartPosition() < startPosition) {
        startPosition = error.getStartPosition();
        endPosition = error.getEndPosition();
      }
    }
  }

  /* ======================================================================== */
  /* Caret management                                                         */
  /* ======================================================================== */

  private int startPosition;
  private int endPosition;
  private int secondStartPosition;
  private int secondEndPosition;
  private int thirdStartPosition;
  private int thirdEndPosition;

  /**
   * Reset caret positions. 
   */
  private void resetCaretPosition() {
    startPosition = Integer.MAX_VALUE;
    endPosition = Integer.MAX_VALUE;
    secondStartPosition = Integer.MAX_VALUE;
    secondEndPosition = Integer.MAX_VALUE;
    thirdStartPosition = Integer.MAX_VALUE;
    thirdEndPosition = Integer.MAX_VALUE;
  }

  /**
   * Move caret.
   * 
   * @param pane MediaWikiPane.
   */
  private void moveCaret(MWPane pane) {
    if (pane == null) {
      return;
    }

    if (startPosition < Integer.MAX_VALUE) {
      pane.setCaretPosition(startPosition);
      pane.moveCaretPosition(endPosition);
    } else if (secondStartPosition < Integer.MAX_VALUE) {
      pane.setCaretPosition(secondStartPosition);
      pane.moveCaretPosition(secondEndPosition);
    } else if (thirdStartPosition < Integer.MAX_VALUE) {
      pane.setCaretPosition(thirdStartPosition);
      pane.moveCaretPosition(thirdEndPosition);
    }
  }

  /* ======================================================================== */
  /* Algorithm management                                                     */
  /* ======================================================================== */

  /**
   * Algorithm. 
   */
  private final CheckErrorAlgorithm algorithm;

  /**
   * @param algo Algorithm
   * @return True if same algorithm.
   */
  public boolean isSameAlgorithm(CheckErrorAlgorithm algo) {
    if ((algo == null) || (algorithm == null)) {
      return false;
    }
    return (algorithm.getErrorNumber() == algo.getErrorNumber());
  }
}
