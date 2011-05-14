/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
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

        // Check 9 digits and compute check
        boolean isISBN = true;
        int check = 0;
        for (int i = 0; i < 9; i++) {
          if ((tmpIndex < contents.length()) && Character.isDigit(contents.charAt(tmpIndex))) {
            check += (10 - i) * (contents.charAt(tmpIndex) - '0');
            tmpIndex++;
          } else {
            isISBN = false;
          }
          if ((tmpIndex < contents.length()) &&
              ((contents.charAt(tmpIndex) == ' ') || (contents.charAt(tmpIndex) == '-'))) {
            tmpIndex++;
          }
        }
        check = check % 11; // Modulus 11
        check = 11 - check; // Invert
        check = check % 11; // 11 -> 0
        char computedCheck = (check < 10) ? (char) ('0' + check): 'X';

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
