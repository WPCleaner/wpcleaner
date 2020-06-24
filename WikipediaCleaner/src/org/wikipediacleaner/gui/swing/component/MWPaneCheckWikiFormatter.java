/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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
   * Format text in a StyleDocument.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  @Override
  public void format(
      StyledDocument doc,
      PageAnalysis pageAnalysis) {
    // Clean formatting
    cleanFormat(doc);

    // Reset caret informations
    resetCaretPosition();

    // Format comments
    defaultFormatElements(doc, pageAnalysis);

    // Format Check Wiki errors
    formatCheckWikiErrors(doc, pageAnalysis);
  }

  /**
   * Format Check Wiki errors in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  private void formatCheckWikiErrors(
      StyledDocument doc,
      PageAnalysis pageAnalysis) {
    if ((doc == null) ||
        (pageAnalysis == null) || (algorithm == null)) {
      return;
    }
    CheckErrorPage errorPage = AlgorithmError.analyzeError(algorithm, pageAnalysis);
    if ((errorPage != null) && (errorPage.getResults() != null)) {
      int lastPosition = -1;
      List<CheckErrorResult> errorResults = errorPage.getResults();
      Collections.sort(errorResults);
      for (CheckErrorResult error : errorResults) {
        if (error.getStartPosition() >= lastPosition) {
          formatCheckWikiError(doc, error);
          lastPosition = error.getEndPosition();
        }
      }
    }
  }

  /**
   * Format a Check Wiki error in a MediaWikiPane.
   * 
   * @param doc Document to be formatted.
   * @param error Check Wiki error to be formatted.
   */
  private void formatCheckWikiError(
      StyledDocument doc,
      CheckErrorResult error) {

    // Basic verifications
    if ((doc == null) || (error == null)) {
      return;
    }

    // Format error
    ConfigurationValueStyle styleConfig = ConfigurationValueStyle.CHECK_WIKI_ERROR;
    if (error.getErrorLevel() == CheckErrorResult.ErrorLevel.CORRECT) {
      styleConfig = ConfigurationValueStyle.CHECK_WIKI_OK;
    } else if (error.getErrorLevel() == CheckErrorResult.ErrorLevel.WARNING) {
      styleConfig = ConfigurationValueStyle.CHECK_WIKI_WARNING;
    }
    doc.setCharacterAttributes(
        error.getStartPosition(),
        error.getLength(),
        doc.getStyle(styleConfig.getName()),
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
  @Override
  protected void moveCaret(MWPane pane) {
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
