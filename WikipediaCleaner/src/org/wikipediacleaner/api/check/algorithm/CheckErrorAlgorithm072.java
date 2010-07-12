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
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 72 of check wikipedia project.
 * Error 72: ISBN wrong checksum in ISBN-10
 */
public class CheckErrorAlgorithm072 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm072() {
    super("ISBN wrong checksum in ISBN-10");
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

        // Check 9 digits and compute check
        boolean isISBN = true;
        int check = 0;
        int i = 0;
        while (isISBN && (tmpIndex < contents.length()) && (i < 9)) {
          if (Character.isDigit(contents.charAt(tmpIndex))) {
            check += (10 - i) * (contents.charAt(tmpIndex) - '0');
            i++;
          } else if ((contents.charAt(tmpIndex) != '-') && (contents.charAt(tmpIndex) != ' ')) {
            isISBN = false;
          }
          tmpIndex++;
        }
        check = check % 11; // Modulus 11
        check = 11 - check; // Invert
        check = check % 11; // 11 -> 0
        char computedCheck = (check < 10) ? (char) ('0' + check): 'X';

        // Removing dashes
        while ((tmpIndex < contents.length()) &&
               ((contents.charAt(tmpIndex) == '-') ||
                (contents.charAt(tmpIndex) == ' '))) {
          tmpIndex++;
        }

        // Verify check
        boolean checkVerified = false;
        if ((tmpIndex < contents.length()) &&
            ((Character.isDigit(contents.charAt(tmpIndex))) ||
             (contents.charAt(tmpIndex) == 'X') ||
             (contents.charAt(tmpIndex) == 'x'))) {
          if (computedCheck == Character.toUpperCase(contents.charAt(tmpIndex))) {
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
          errors.add(errorResult);
          result = true;
        }
      }
    }
    return result;
  }
}
