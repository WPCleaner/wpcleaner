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
 * Algorithm for analyzing error 28 of check wikipedia project.
 * Error 28: Table not correct end
 */
public class CheckErrorAlgorithm028 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm028() {
    super("Table not correct end");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }
    String contents = analysis.getContents();
    int startIndex = contents.length();
    boolean result = false;
    int beginIndex = contents.lastIndexOf("{|", startIndex);
    int endIndex = contents.lastIndexOf("|}", startIndex);
    int count = 0;
    while (startIndex > 0) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        startIndex = 0;
      } else if ((endIndex >= 0) && ((beginIndex < endIndex) || (beginIndex < 0))) {
        if (shouldCount(analysis, endIndex)) {
          count++;
        }
        startIndex = endIndex;
        endIndex = contents.lastIndexOf("|}", startIndex - 1);
      } else {
        if (shouldCount(analysis, beginIndex)) {
          count--;
          if (count < 0) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(createCheckErrorResult(
                analysis.getPage(), beginIndex, beginIndex + 2));
            count = 0;
          }
        }
        startIndex = beginIndex;
        beginIndex = contents.lastIndexOf("{|", startIndex - 1);
      }
    }
    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param index Current index.
   * @return True if this place should count for the detection.
   */
  private boolean shouldCount(PageAnalysis analysis, int index) {
    if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_CODE, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null)) {
      return false;
    }
    return true;
  }
}
