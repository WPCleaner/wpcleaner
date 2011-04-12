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
 * Algorithm for analyzing error 42 of check wikipedia project.
 * Error 42: HTML text style element &lt;small&gt;
 */
public class CheckErrorAlgorithm042 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm042() {
    super("HTML text style element <small>");
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

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {

      // Searching for next <small>
      int beginIndex = contents.indexOf("<small>", startIndex);
      if (beginIndex < 0) {
        startIndex = contents.length();
      } else {
        int currentPos = beginIndex + 7;
        int levelSmall = 1;
        while ((currentPos < contents.length()) && (levelSmall > 0)) {
          if (contents.charAt(currentPos) == '<') {
            if (contents.startsWith("<small>", currentPos)) {
              levelSmall++;
              currentPos += 6;
            } else if (contents.startsWith("</small>", currentPos)) {
              levelSmall--;
              if (levelSmall == 0) {
                if (errors == null) {
                  return true;
                }
                result = true;
                CheckErrorResult errorResult = createCheckErrorResult(
                    page, beginIndex, currentPos + 8);
                errors.add(errorResult);
              }
              currentPos += 7;
            }
          }
          currentPos++;
        }
        startIndex = currentPos;
      }
    }
    return result;
  }
}
