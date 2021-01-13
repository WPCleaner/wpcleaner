/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a074;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 74 of check wikipedia project.
 * Error 74: Link with no target
 */
public class CheckErrorAlgorithm074 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm074() {
    super("Link with no target");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    for (PageElementInternalLink link : analysis.getInternalLinks()) {
      if (link.getFullLink().trim().length() == 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        String text = link.getText();
        if ((text != null) && (text.length() > 0)) {
          errorResult.addReplacement("[[" + link.getText() + "]]");
        }
        errors.add(errorResult);
      }
    }

    return result;
  }
}
