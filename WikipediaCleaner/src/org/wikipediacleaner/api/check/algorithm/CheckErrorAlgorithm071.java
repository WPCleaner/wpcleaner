/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;


/**
 * Algorithm for analyzing error 71 of check wikipedia project.
 * Error 71: ISBN wrong position of X
 */
public class CheckErrorAlgorithm071 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm071() {
    super("ISBN wrong position of X");
  }

  /**
   * @param analysis Page analysis. 
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.PageAnalysis, java.util.Collection)
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      String isbnNumber = isbn.getISBN();
      if (isbnNumber != null) {
        boolean found = false;
        for (int i = 0; i < isbnNumber.length(); i++) {
          if (Character.toUpperCase(isbnNumber.charAt(i)) == 'X') {
            if ((i != 9) || (isbnNumber.length() != 10)) {
              found = true;
            }
          }
        }

        if (found) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), isbn.getBeginIndex(), isbn.getEndIndex());
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
