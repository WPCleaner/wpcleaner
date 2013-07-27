/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 73 of check wikipedia project.
 * Error 73: ISBN wrong checksum in ISBN-13
 */
public class CheckErrorAlgorithm073 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm073() {
    super("ISBN wrong checksum in ISBN-13");
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
    int startIndex = -1;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf("ISBN ", startIndex + 1);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        // Removing white spaces
        int tmpIndex = startIndex + 5;
        while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }

        // Check 12 digits and compute check
        boolean isISBN = true;
        int check = 0;
        if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '9')) {
          check += (contents.charAt(tmpIndex) - '0');
          tmpIndex++;
        } else {
          isISBN = false;
        }
        if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '7')) {
          check += 3 * (contents.charAt(tmpIndex) - '0');
          tmpIndex++;
        } else {
          isISBN = false;
        }
        if ((tmpIndex < contents.length()) &&
            ((contents.charAt(tmpIndex) == '8') ||
             (contents.charAt(tmpIndex) == '9'))) {
          check += (contents.charAt(tmpIndex) - '0');
          tmpIndex++;
        }
        if ((tmpIndex < contents.length()) &&
            ((contents.charAt(tmpIndex) == ' ') || (contents.charAt(tmpIndex) == '-'))) {
          tmpIndex++;
        }
        for (int i = 0; i < 9; i++) {
          if ((tmpIndex < contents.length()) && Character.isDigit(contents.charAt(tmpIndex))) {
            check += ((i % 2) == 0 ? 3 : 1) * (contents.charAt(tmpIndex) - '0');
            tmpIndex++;
          } else {
            isISBN = false;
          }
          if ((tmpIndex < contents.length()) &&
              ((contents.charAt(tmpIndex) == ' ') || (contents.charAt(tmpIndex) == '-'))) {
            tmpIndex++;
          }
        }
        check = check % 10; // Modulus 10
        check = 10 - check; // Invert
        check = check % 10; // 10 -> 0
        char computedCheck = (char) ('0' + check);

        // Verify check
        boolean checkVerified = false;
        char checkCharacter = ' ';
        if ((tmpIndex < contents.length()) &&
            ((Character.isDigit(contents.charAt(tmpIndex))) ||
             (contents.charAt(tmpIndex) == 'X') ||
             (contents.charAt(tmpIndex) == 'x'))) {
          checkCharacter = contents.charAt(tmpIndex);
          if (computedCheck == Character.toUpperCase(checkCharacter)) {
            checkVerified = true;
          }
        } else {
          isISBN = false;
        }
        tmpIndex++;

        // Verify end of ISBN
        while ((tmpIndex < contents.length()) &&
               ((contents.charAt(tmpIndex) == '-') ||
                (contents.charAt(tmpIndex) == ' '))) {
          tmpIndex++;
        }
        if ((tmpIndex < contents.length()) &&
            (Character.isLetterOrDigit(contents.charAt(tmpIndex)))) {
          isISBN = false;
        }

        // Check result
        if (isISBN && !checkVerified) {
          if (errors == null) {
            return true;
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), startIndex, tmpIndex);
          errorResult.addPossibleAction(
              GT._(
                  "The checksum is {0} instead of {1}",
                  new Object[] { checkCharacter, computedCheck } ),
              new NullActionProvider());
          errors.add(errorResult);
          result = true;
        }
      }
    }
    return result;
  }
}
