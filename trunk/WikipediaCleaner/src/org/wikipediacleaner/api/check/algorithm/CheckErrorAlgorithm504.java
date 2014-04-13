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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 504 of check wikipedia project.
 * Error 504: Reference in title
 */
public class CheckErrorAlgorithm504 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm504() {
    super("Reference in title");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check every reference
    Collection<PageElementTag> refs = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTag ref : refs) {
      PageElementTitle title = analysis.isInTitle(ref.getCompleteBeginIndex());
      if (title != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult error = createCheckErrorResult(
            analysis.getPage(),
            ref.getCompleteBeginIndex(), ref.getCompleteEndIndex());
        errors.add(error);
      }
    }

    return result;
  }
}
