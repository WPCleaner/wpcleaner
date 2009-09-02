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
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 46 of check wikipedia project.
 * Error 46: Square brackets not correct begin
 */
public class CheckErrorAlgorithm46 extends CheckErrorAlgorithmBase {

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#getErrorDescription()
   */
  public String getErrorDescription() {
    return GT._("Square brackets not correct begin");
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
    int beginIndex = contents.indexOf("[[", startIndex);
    int endIndex = contents.indexOf("]]", startIndex);
    int count = 0;
    while (startIndex < contents.length()) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        startIndex = contents.length();
      } else if ((beginIndex > 0) && ((beginIndex < endIndex) || (endIndex < 0))) {
        count++;
        startIndex = beginIndex + 2;
        beginIndex = contents.indexOf("[[", startIndex);
      } else {
        count--;
        if (count < 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          errors.add(new CheckErrorResult(endIndex, endIndex + 2));
          count = 0;
        }
        startIndex = endIndex + 2;
        endIndex = contents.indexOf("]]", startIndex);
      }
    }
    return result;
  }
}
