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

package org.wikipediacleaner.api.check;

import java.util.ArrayList;

import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 8 of check wikipedia project.
 * Error 8: Headline should end with "="
 */
public class CheckErrorAlgorithm8 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm8() {
    super("Headline should end with \"=\"");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      int titleIndex = contents.indexOf("=", startIndex);
      if (titleIndex < 0) {
        startIndex = contents.length();
      } else {
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
          currentPos = endLineIndex - 1;
          while ((currentPos >= 0) && (contents.charAt(currentPos) == ' ')) {
            currentPos--;
          }
          while ((currentPos >= 0) && (contents.charAt(currentPos) == '=')) {
            currentPos--;
            titleLevel--;
          }
          if (titleLevel > 0) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(new CheckErrorResult(getShortDescription(), titleIndex, endLineIndex));
          }
          startIndex = endLineIndex + 1;
        } else {
          if (endLineIndex < 0) {
            startIndex = contents.length();
          } else {
            startIndex = endLineIndex;
          }
        }
      }
    }
    return result;
  }
}
