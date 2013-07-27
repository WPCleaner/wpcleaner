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


/**
 * Algorithm for analyzing error 12 of check wikipedia project.
 * Error 12: HTML List elements
 */
public class CheckErrorAlgorithm012 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm012() {
    super("HTML List elements");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }
    boolean result = false;
    result = addTags(result, pageAnalysis, errors, PageElementTag.TAG_HTML_OL);
    result = addTags(result, pageAnalysis, errors, PageElementTag.TAG_HTML_UL);
    result = addTags(result, pageAnalysis, errors, PageElementTag.TAG_HTML_LI);
    return result;
  }
}
