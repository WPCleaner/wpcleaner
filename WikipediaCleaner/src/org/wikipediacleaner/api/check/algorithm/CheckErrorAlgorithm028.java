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
    String contents = pageAnalysis.getContents();
    int startIndex = contents.length();
    boolean result = false;
    int beginIndex = contents.lastIndexOf("{|", startIndex);
    int endIndex = contents.lastIndexOf("|}", startIndex);
    int count = 0;
    while (startIndex > 0) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        startIndex = 0;
      } else if ((endIndex >= 0) && ((beginIndex < endIndex) || (beginIndex < 0))) {
        count++;
        startIndex = endIndex;
        endIndex = contents.lastIndexOf("|}", startIndex - 1);
      } else {
        count--;
        if (count < 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          errors.add(createCheckErrorResult(
              pageAnalysis.getPage(), beginIndex, beginIndex + 2));
          count = 0;
        }
        startIndex = beginIndex;
        beginIndex = contents.lastIndexOf("{|", startIndex - 1);
      }
    }
    return result;
  }
}
