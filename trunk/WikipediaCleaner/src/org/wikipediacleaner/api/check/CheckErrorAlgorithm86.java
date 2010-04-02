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
 * Algorithm for analyzing error 86 of check wikipedia project.
 * Error 86: External link with two brackets
 */
public class CheckErrorAlgorithm86 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm86() {
    super("Title linked in text");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      // Looking for [[
      startIndex = contents.indexOf("[[", startIndex);
      if (startIndex >= 0) {
        int linkIndex = startIndex + 2;
        // Removing possible whitespaces before link
        while ((linkIndex < contents.length()) && (contents.charAt(linkIndex) == ' ')) {
          linkIndex++;
        }
        if ((contents.startsWith("http://", linkIndex)) ||
            (contents.startsWith("https://", linkIndex))) {
          int endIndex = contents.indexOf("]", linkIndex);
          if (endIndex < 0) {
            startIndex = contents.length();
          } else {
            if ((endIndex + 1 < contents.length()) &&
                (contents.charAt(endIndex + 1) == ']')) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = new CheckErrorResult(getShortDescription(), startIndex, endIndex + 2);
              errorResult.addReplacement(contents.substring(startIndex + 1, endIndex + 1));
              errors.add(errorResult);
              startIndex = endIndex + 2;
            } else {
              startIndex = linkIndex;
            }
          }
        } else {
          startIndex = linkIndex;
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
