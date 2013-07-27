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
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 42 of check wikipedia project.
 * Error 42: HTML text style element &lt;small&gt;
 */
public class CheckErrorAlgorithm042 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm042() {
    super("HTML text style element <small>");
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

    // Analyzing the text from the beginning
    Collection<PageElementTag> tags = pageAnalysis.getTags(PageElementTag.TAG_HTML_SMALL);
    if (tags == null) {
      return false;
    }
    int level = 0;
    boolean result = false;
    for (PageElementTag tag : tags) {
      if (tag.isFullTag()) {
        // Full tag
        if (level == 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              tag.getBeginIndex(), tag.getEndIndex());
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      } else if (tag.isEndTag()) {
        // Closing tag
        level--;
        if (level < 0) {
          level = 0;
        }
      } else {
        level++;
        if (level == 1) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
