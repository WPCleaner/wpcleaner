/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 503 of check wikipedia project.
 * Error 503: Internal link in title
 */
public class CheckErrorAlgorithm503 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm503() {
    super("Internal link in title");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (pageAnalysis == null) {
      return false;
    }

    // Check every internal link
    Collection<PageElementInternalLink> links = pageAnalysis.getInternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementInternalLink link : links) {
      PageElementTitle title = pageAnalysis.isInTitle(link.getBeginIndex());
      if (title != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult error = createCheckErrorResult(
            pageAnalysis.getPage(),
            link.getBeginIndex(), link.getEndIndex());
        error.addReplacement(link.getDisplayedText());
        errors.add(error);
      }
    }

    return result;
  }
}
