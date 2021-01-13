/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a075;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 75 of check wikipedia project.
 * Error 75: Indented list
 */
public class CheckErrorAlgorithm075 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm075() {
    super("Indented list");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = -1;
    String contents = analysis.getContents();
    String previousLine = "";
    String incorrectLine = null;
    while (startIndex < contents.length()) {
      if (startIndex < 0) {
        startIndex = 0;
      } else {
        startIndex = contents.indexOf('\n', startIndex);
        if (startIndex >= 0) {
          startIndex++;
        }
      }
      if ((startIndex < 0) || startIndex >= contents.length()) {
        startIndex = contents.length();
      } else {
        if (contents.charAt(startIndex) == ':') {
          int endIndex = startIndex;
          boolean otherThanColon = false;
          while ((startIndex < contents.length()) &&
                 (":-*#".indexOf(contents.charAt(endIndex)) >= 0)) {
            if (contents.charAt(endIndex) != ':') {
              otherThanColon = true;
            }
            endIndex++;
          }
          String newLine = contents.substring(startIndex, endIndex);
          int newLength = newLine.length();
          if ((newLength > 1) && otherThanColon &&
              ((newLength > previousLine.length() + 1) ||
               (!previousLine.startsWith(newLine.substring(0, newLength - 1))))) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, startIndex, endIndex);
            char lastChar = newLine.charAt(newLine.length() - 1);
            if (incorrectLine == null) {
              incorrectLine = newLine;
            }
            if (newLine.equals(incorrectLine)) {
              if (newLine.length() > previousLine.length() + 1) {
                errorResult.addReplacement(previousLine + lastChar);
              } else {
                errorResult.addReplacement(previousLine.substring(0, newLength - 1) + lastChar);
              }
            }
            errors.add(errorResult);
          } else {
            previousLine = newLine;
            incorrectLine = null;
          }
        } else {
          previousLine = "";
        }
      }
    }
    return result;
  }
}
