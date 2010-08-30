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
    int startIndex = -1;
    while (startIndex < contents.length()) {
      if (startIndex < 0) {
        startIndex = 0;
      } else {
        startIndex = contents.indexOf('\n', startIndex);
        if (startIndex >= 0) {
          startIndex++;
        }
      }
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        if ((startIndex + 1 < contents.length()) &&
            (contents.charAt(startIndex) == ':')) {
          char nextChar = contents.charAt(startIndex + 1);
          if ((nextChar == '-') ||
              (nextChar == '*') ||
              (nextChar == '#')) {
            if (errors == null) {
              return true;
            }
            result = true;
            int nextLine = contents.indexOf('\n', startIndex);
            if (nextLine < 0) {
              nextLine = contents.length();
            }
            CheckErrorResult errorResult = createCheckErrorResult(page, startIndex, nextLine);
            errors.add(errorResult);
          }
        }
      }
    }
    return result;
  }
}
