/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 42 of check wikipedia project.
 * Error 42: HTML text style element &lt;small&gt;
 */
public class CheckErrorAlgorithm042_Old extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm042_Old() {
    super("HTML text style element <small>");
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
    Collection<PageElementTag> tags = analysis.getTags(HtmlTagType.SMALL);
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
              analysis,
              tag.getBeginIndex(), tag.getEndIndex());
          errorResult.addReplacement("", GT._T("Delete"));
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
              analysis,
              tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
