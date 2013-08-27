/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 05 of check wikipedia project.
 * Error 05: Found a comment "&lt;!--" with no "--&gt;" end.
 */
public class CheckErrorAlgorithm005 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm005() {
    super("Found a comment \"<!--\" with no \"-->\" end.");
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

    // Analyze contents from the end
    boolean result = false;
    Page page = pageAnalysis.getPage();
    String contents = pageAnalysis.getContents();
    boolean inComment = false;
    int possibleEndIndex = -1;
    int previousStartIndex = -1;
    int currentIndex = contents.length();
    while (currentIndex > 0) {
      currentIndex--;
      if (contents.startsWith("-->", currentIndex)) {
        inComment = true;
        possibleEndIndex = -1;
        previousStartIndex = -1;
        currentIndex -= 4;
      } else if (contents.startsWith("->", currentIndex)) {
        possibleEndIndex = currentIndex;
      } else if (contents.startsWith("<!--", currentIndex)) {
        if (inComment) {
          inComment = false;
        } else {
          if (errors == null) {
            return true;
          }
          result = true;
          int nextIndex = currentIndex + 4;
          while ((nextIndex < contents.length()) &&
                 (contents.charAt(nextIndex) == ' ')) {
            nextIndex++;
          }
          CheckErrorResult errorResult = null;
          if (possibleEndIndex > 0) {
            errorResult = createCheckErrorResult(
                page, currentIndex, possibleEndIndex + 2);
            errorResult.addReplacement(
                contents.substring(currentIndex, possibleEndIndex) + "-->",
                GT._("Properly end the comment"));
          } else if (previousStartIndex > 0) {
            errorResult = createCheckErrorResult(
                page, currentIndex, previousStartIndex);
            errorResult.addReplacement(
                contents.substring(currentIndex, previousStartIndex) + "-->",
                GT._("Properly end the comment"));
            errorResult.addReplacement(
                contents.substring(nextIndex, previousStartIndex),
                GT._("Uncomment"));
          } else {
            errorResult = createCheckErrorResult(
                page, currentIndex, nextIndex);
            errorResult.addReplacement("", GT._("Uncomment"));
          }
          errors.add(errorResult);
        }
        previousStartIndex = currentIndex;
      }
    }

    return result;
  }
}
