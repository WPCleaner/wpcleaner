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
import org.wikipediacleaner.api.data.PageElementExternalLink;


/**
 * Algorithm for analyzing error 86 of check wikipedia project.
 * Error 86: External link with two brackets
 */
public class CheckErrorAlgorithm086 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm086() {
    super("External link with two brackets");
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

    int startIndex = 0;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      // Looking for [[
      startIndex = contents.indexOf("[[", startIndex);
      if (startIndex >= 0) {
        int linkIndex = startIndex + 2;
        // Removing possible white spaces before link
        while ((linkIndex < contents.length()) && (contents.charAt(linkIndex) == ' ')) {
          linkIndex++;
        }
        boolean protocolFound = PageElementExternalLink.isPossibleProtocol(contents, linkIndex);
        if (protocolFound) {
          int endIndex = contents.indexOf("]", linkIndex);
          if (endIndex < 0) {
            startIndex = contents.length();
          } else {
            if ((endIndex + 1 < contents.length()) &&
                (contents.charAt(endIndex + 1) == ']')) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), startIndex, endIndex + 2);
              errorResult.addReplacement(contents.substring(startIndex + 1, endIndex + 1));
              errors.add(errorResult);
              startIndex = endIndex + 2;
            } else {
              startIndex = linkIndex;
            }
          }
        } else {
          startIndex = linkIndex;
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
