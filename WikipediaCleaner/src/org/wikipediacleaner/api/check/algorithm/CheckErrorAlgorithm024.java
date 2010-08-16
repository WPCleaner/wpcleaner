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
 * Algorithm for analyzing error 24 of check wikipedia project.
 * Error 24: Pre not correct end
 */
public class CheckErrorAlgorithm024 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm024() {
    super("Pre not correct end");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyze contents from the beginning
    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf("<pre>", startIndex);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        int endIndex = contents.indexOf("</pre>", startIndex);
        if (endIndex < 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          endIndex = startIndex + "<pre>".length();
          CheckErrorResult errorResult = createCheckErrorResult(
              page, startIndex, endIndex);
          errors.add(errorResult);
          startIndex = endIndex;
        } else {
          startIndex = endIndex;
        }
      }
    }
    return result;
  }
}
