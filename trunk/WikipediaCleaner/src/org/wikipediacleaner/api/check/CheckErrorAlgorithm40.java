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
 * Algorithm for analyzing error 40 of check wikipedia project.
 * Error 40: HTML text style element &lt;font&gt;
 */
public class CheckErrorAlgorithm40 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm40() {
    super("HTML text style element <font>");
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
      if (contents.charAt(startIndex) == '<') {
        int endIndex = startIndex;
        if (contents.startsWith("<font", startIndex)) {
          int currentPos = startIndex + 1;
          while ((currentPos < contents.length()) &&
                 (contents.charAt(currentPos) != '>')) {
            boolean ok = false;
            if (contents.charAt(currentPos) != '<') {
              ok = true;
            }
            if (ok) {
              currentPos++;
            } else {
              currentPos = contents.length();
            }
          }
          if ((currentPos < contents.length()) && (contents.charAt(currentPos) == '>')) {
            endIndex = currentPos + 1;
          }
        } else if (contents.startsWith("</font>", startIndex)) {
          endIndex += "</font>".length();
        }
        if (endIndex > startIndex) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = new CheckErrorResult(
              getShortDescription(), startIndex, endIndex);
          errors.add(errorResult);
        }
      }
      startIndex++;
    }
    return result;
  }
}
