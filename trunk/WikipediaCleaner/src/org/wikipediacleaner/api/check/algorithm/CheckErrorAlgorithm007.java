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
 * Algorithm for analyzing error 7 of check wikipedia project.
 * Error 7: Headlines all start with three "="
 */
public class CheckErrorAlgorithm007 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm007() {
    super("Headlines all start with three \"=\"");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {

      // Find next =
      int titleIndex = contents.indexOf("=", startIndex);
      if (titleIndex < 0) {
        startIndex = contents.length();
      } else {

        // Check if the = is the beginning of a title
        int endLineIndex = contents.indexOf("\n", titleIndex);
        if ((titleIndex == 0) || (contents.charAt(titleIndex - 1) == '\n')) {
          int titleLevel = 0;
          int currentPos = titleIndex;
          while ((currentPos < contents.length()) && (contents.charAt(currentPos) == '=')) {
            currentPos++;
            titleLevel++;
          }
          if (endLineIndex < 0) {
            endLineIndex = contents.length();
          }
          if (titleLevel != 3) {
            return false;
          }

          // The first title is at level 3, check if there's a lower title later
          startIndex = endLineIndex + 1;
          while (startIndex < contents.length()) {
            int nextTitleIndex = contents.indexOf("=", startIndex);
            if (nextTitleIndex < 0) {
              startIndex = contents.length();
            } else {
              int nextEndLineIndex = contents.indexOf("\n", nextTitleIndex);
              if (contents.charAt(nextTitleIndex - 1) == '\n') {
                titleLevel = 0;
                currentPos = nextTitleIndex;
                while ((currentPos < contents.length()) && (contents.charAt(currentPos) == '=')) {
                  currentPos++;
                  titleLevel++;
                }
                if (nextEndLineIndex < 0) {
                  nextEndLineIndex = contents.length();
                }
                if (titleLevel < 3) {
                  return false;
                }
              }
              startIndex = nextEndLineIndex + 1;
            }
          }
          if (errors == null) {
            return true;
          }
          result = true;
          errors.add(createCheckErrorResult(page, titleIndex, endLineIndex));
          return true;
        }
        if (endLineIndex < 0) {
          startIndex = contents.length();
        } else {
          startIndex = endLineIndex;
        }
      }
    }
    return result;
  }
}
