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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementComment;


/**
 * Algorithm for analyzing error 69 of check wikipedia project.
 * Error 69: ISBN wrong syntax
 */
public class CheckErrorAlgorithm069 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm069() {
    super("ISBN wrong syntax");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments in the page contents.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyze contents from the beginning
    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf("ISBN", startIndex);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        // Check following characters
        int tmpIndex = startIndex + 4;
        if (tmpIndex < contents.length()) {
          if ((contents.startsWith("-10", tmpIndex)) ||
              (contents.startsWith("-13", tmpIndex))) {
            int tmpIndex2 = startIndex - 1;
            while ((tmpIndex2 >= 0) && (contents.charAt(tmpIndex2) == ' ')) {
              tmpIndex2--;
            }
            if ((tmpIndex2 < 1) ||
                (!contents.startsWith("{{", tmpIndex2 - 1))) {
              if (errors == null) {
                return true;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, startIndex, tmpIndex + 3);
              errors.add(errorResult);
              result = true;
            }
          }
        }
        startIndex = tmpIndex;
      }
    }
    return result;
  }
}
