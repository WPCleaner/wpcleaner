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
 * Algorithm for analyzing error 28 of check wikipedia project.
 * Error 28: Table not correct end
 */
public class CheckErrorAlgorithm028 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm028() {
    super("Table not correct end");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    int startIndex = contents.length();
    boolean result = false;
    int beginIndex = contents.lastIndexOf("{|", startIndex);
    int endIndex = contents.lastIndexOf("|}", startIndex);
    int count = 0;
    while (startIndex > 0) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        startIndex = 0;
      } else if ((endIndex >= 0) && ((beginIndex < endIndex) || (beginIndex < 0))) {
        count++;
        startIndex = endIndex;
        endIndex = contents.lastIndexOf("|}", startIndex - 1);
      } else {
        count--;
        if (count < 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          errors.add(createCheckErrorResult(page, beginIndex, beginIndex + 2));
          count = 0;
        }
        startIndex = beginIndex;
        beginIndex = contents.lastIndexOf("{|", startIndex - 1);
      }
    }
    return result;
  }
}
