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
 * Algorithm for analyzing error 54 of check wikipedia project.
 * Error 54: Break in list
 */
public class CheckErrorAlgorithm054 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm054() {
    super("Break in list");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int endLineIndex = -1;
    while (endLineIndex + 1 < contents.length()) {

      // Check if the next line is a list
      boolean isList = (contents.charAt(endLineIndex + 1) == '*');

      // Searching next end line
      endLineIndex = contents.indexOf("\n", endLineIndex + 1);
      if (endLineIndex < 0) {
        endLineIndex = contents.length();
      }

      // Checking if the line ends with a <br />
      if (isList) {
        boolean found = true;
        int currentPos = endLineIndex - 1;
        while ((currentPos >= 0) && (Character.isWhitespace(contents.charAt(currentPos)))) {
          currentPos--;
        }
        if ((currentPos >= 0) && (contents.charAt(currentPos) == '>')) {
          currentPos--;
        } else {
          found = false;
        }
        if ((currentPos >= 0) && (contents.charAt(currentPos) == '/')) {
          currentPos--;
        }
        while ((currentPos >= 0) && (Character.isWhitespace(contents.charAt(currentPos)))) {
          currentPos--;
        }
        if ((currentPos < 2) || (!contents.startsWith("<br", currentPos - 2))) {
          found = false;
        }
        currentPos -= 2;
        if (found) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = new CheckErrorResult(
              getShortDescription(), currentPos, endLineIndex);
          errorResult.addReplacement("");
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
