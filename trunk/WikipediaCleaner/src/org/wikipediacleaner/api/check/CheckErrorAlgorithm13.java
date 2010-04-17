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
 * Algorithm for analyzing error 13 of check wikipedia project.
 * Error 13: Math not correct end
 */
public class CheckErrorAlgorithm13 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm13() {
    super("Math not correct end");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyze contents from the beginning
    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf("<math>", startIndex);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        int endIndex = contents.indexOf("</math>", startIndex);
        if (endIndex < 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          endIndex = startIndex + "<math>".length();
          CheckErrorResult errorResult = new CheckErrorResult(
              getShortDescription(), startIndex, endIndex);
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
