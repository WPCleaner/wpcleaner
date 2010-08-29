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

import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 27 of check wikipedia project.
 * Error 27: Unicode syntax
 */
public class CheckErrorAlgorithm027 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm027() {
    super("Unicode syntax");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf('&', startIndex);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        int beginIndex = startIndex;
        startIndex++;
        if ((startIndex < contents.length()) &&
            (contents.charAt(startIndex) == '#')) {
          startIndex++;
        }
        int startNumber = startIndex;
        boolean digitFound = false;
        while ((startIndex < contents.length()) &&
               (Character.isDigit(contents.charAt(startIndex)))) {
          startIndex++;
          digitFound = true;
        }
        if ((digitFound) &&
            (startIndex < contents.length()) &&
            (contents.charAt(startIndex) == ';')) {
          startIndex++;
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(page, beginIndex, startIndex);
          try {
            int codePoint = Integer.parseInt(contents.substring(startNumber, startIndex - 1));
            if (Character.isValidCodePoint(codePoint)) {
              String replacement = new String(Character.toChars(codePoint));
              errorResult.addReplacement(replacement, GT._("Replace with {0}", replacement));
            }
          } catch (NumberFormatException e) {
            //
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
