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
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 72 of check wikipedia project.
 * Error 72: ISBN wrong checksum in ISBN-10
 */
public class CheckErrorAlgorithm072 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm072() {
    super("ISBN wrong checksum in ISBN-10");
  }

  /**
   * @return Flag indicating if the detection is fully done.
   */
  @Override
  public boolean isFullDetection() {
    return false;
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

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      String number = isbn.getISBN();
      if ((number != null) && (number.length() == 10)) {
        char check = number.charAt(9);
        char computedCheck = isbn.getCheck();
        if ((check != computedCheck) &&
            (Character.isDigit(computedCheck) || (computedCheck == 'X'))) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), isbn.getBeginIndex(), isbn.getEndIndex());
          errorResult.addPossibleAction(
              GT._(
                  "The checksum is {0} instead of {1}",
                  new Object[] { check, computedCheck } ),
              new NullActionProvider());
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
