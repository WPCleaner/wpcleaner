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

    // Analyze contents from the beginning
    int startIndex = 0;
    boolean inComment = false;
    int commentIndex = -1;
    int possibleEndIndex = -1;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      if (inComment) {
        if (contents.startsWith("-->", startIndex)) {
          inComment = false;
          startIndex += 2;
        } else if (contents.startsWith("->", startIndex)) {
          if (possibleEndIndex == -1) {
            possibleEndIndex = startIndex;
          }
        }
      } else {
        if (contents.startsWith("<!--", startIndex)) {
          inComment = true;
          commentIndex = startIndex;
          possibleEndIndex = -1;
          startIndex += 3;
        }
      }
      startIndex++;
    }
    if (!inComment) {
      return false;
    }
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = null;
    if (possibleEndIndex < 0) {
      errorResult = createCheckErrorResult(
          pageAnalysis.getPage(), commentIndex, commentIndex + 4);
    } else {
      errorResult = createCheckErrorResult(
          pageAnalysis.getPage(), commentIndex, possibleEndIndex + 2);
      errorResult.addReplacement(
          contents.substring(commentIndex, possibleEndIndex) + "-->");
    }
    errors.add(errorResult);
    return true;
  }
}
