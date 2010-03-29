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
 * Algorithm for analyzing error 46 of check wikipedia project.
 * Error 46: Square brackets not correct begin
 */
public class CheckErrorAlgorithm46 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm46() {
    super("Square brackets not correct begin");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyze contents from the beginning by counting [[ and ]]
    int startIndex = 0;
    boolean result = false;
    int beginIndex = contents.indexOf("[[", startIndex);
    int endIndex = contents.indexOf("]]", startIndex);
    int count = 0;
    while (startIndex < contents.length()) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        // No more [[ or ]]
        startIndex = contents.length();
      } else if ((beginIndex >= 0) && ((beginIndex < endIndex) || (endIndex < 0))) {
        // Found a [[
        count++;
        startIndex = beginIndex + 2;
        beginIndex = contents.indexOf("[[", startIndex);
      } else {
        // Found a ]]
        count--;
        if (count < 0) {
          // Found more ]] than [[
          if (errors == null) {
            return true;
          }
          result = true;

          // Check if the situation is something like [....]] (replacement: [[....]])
          boolean errorReported = false;
          int previousBegin = contents.lastIndexOf('[', endIndex - 1);
          if (previousBegin > 0) {
            int previousCR = contents.lastIndexOf('\n', endIndex - 1);
            int previousEnd = contents.lastIndexOf(']', endIndex - 1);
            if (((previousCR < 0) || (previousCR < previousBegin)) &&
                ((previousEnd < 0) || (previousEnd < previousBegin))) {
              CheckErrorResult errorResult = new CheckErrorResult(
                  getShortDescription(), previousBegin, endIndex + 2);
              errorResult.addReplacement("[" + contents.substring(previousBegin, endIndex + 2));

              // Check if the situation is something like [http://....]] (replacement: [http://....]) 
              if (contents.startsWith("http://", previousBegin + 1)) {
                errorResult.addReplacement(contents.substring(previousBegin, endIndex + 1));
              }

              errors.add(errorResult);
              errorReported = true;
            }
          }

          // Default
          if (!errorReported) {
            errors.add(new CheckErrorResult(getShortDescription(), endIndex, endIndex + 2));
          }
          count = 0;
        }
        startIndex = endIndex + 2;
        endIndex = contents.indexOf("]]", startIndex);
      }
    }
    return result;
  }
}
