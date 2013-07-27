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
 * Algorithm for analyzing error 69 of check wikipedia project.
 * Error 69: ISBN wrong syntax
 */
public class CheckErrorAlgorithm069 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm069() {
    super("ISBN wrong syntax");
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

    // Analyze contents from the beginning
    int startIndex = 0;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf("ISBN", startIndex);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        // Check following characters
        int tmpIndex = startIndex + 4;
        if (tmpIndex < contents.length()) {
          if ((contents.startsWith("-10", tmpIndex)) ||
              (contents.startsWith("-13", tmpIndex))) {
            int tmpIndex2 = startIndex - 1;
            while ((tmpIndex2 >= 0) && (contents.charAt(tmpIndex2) == ' ')) {
              tmpIndex2--;
            }
            if ((tmpIndex2 < 1) ||
                (!contents.startsWith("{{", tmpIndex2 - 1))) {
              if (errors == null) {
                return true;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), startIndex, tmpIndex + 3);
              errors.add(errorResult);
              result = true;
            }
          }
        }
        startIndex = tmpIndex;
      }
    }
    return result;
  }
}
