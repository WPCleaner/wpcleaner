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
 * Algorithm for analyzing error 55 of check wikipedia project.
 * Error 55: HTML text style element &lt;small&gt; double
 */
public class CheckErrorAlgorithm55 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm55() {
    super("HTML text style element <small> double");
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
    int startIndex = 0;
    int beginIndex = -1;
    int endIndex = -1;
    int level = 0;
    while (startIndex < contents.length()) {
      // Update position of next <small>
      if (beginIndex < startIndex) {
        beginIndex = contents.indexOf("<small>", startIndex);
      }
      // Update position of next </small>
      if (endIndex < startIndex) {
        endIndex = contents.indexOf("</small>", startIndex);
      }
      if ((beginIndex < 0) || (endIndex < 0)) {
        // No more elements
        startIndex = contents.length();
      } else if (beginIndex < endIndex) {
        // Next element is <small>
        level++;
        if (level > 1) {
          if (errors == null) {
            return true;
          }
          result = true;
          errors.add(new CheckErrorResult(getShortDescription(), beginIndex, endIndex + "</small>".length()));
        }
        startIndex = beginIndex + 1;
      } else {
        // Next element is </small>
        level--;
        startIndex = endIndex + 1;
      }
    }
    return result;
  }
}
