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

import java.util.ArrayList;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.Page;
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

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyze contents from the beginning
    int startIndex = -1;
    boolean result = false;
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
          CheckErrorResult errorResult = new CheckErrorResult(
              getShortDescription(), startIndex, tmpIndex);
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
