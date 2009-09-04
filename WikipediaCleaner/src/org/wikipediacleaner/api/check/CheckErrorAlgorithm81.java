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
 * Algorithm for analyzing error 81 of check wikipedia project.
 * Error 81: Reference duplication.
 */
public class CheckErrorAlgorithm81 extends CheckErrorAlgorithmBase {

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#getErrorDescription()
   */
  public String getErrorDescription() {
    return GT._("Reference duplication");
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
      int beginIndex = contents.indexOf("<ref>", startIndex);
      if (beginIndex < 0) {
        return result;
      }
      int endIndex = contents.indexOf("</ref>", beginIndex);
      if (endIndex < 0) {
        return result;
      }
      endIndex += 6;
      int duplicateIndex = contents.indexOf(contents.substring(beginIndex, endIndex), endIndex);
      if (duplicateIndex >= 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        errors.add(new CheckErrorResult(beginIndex, endIndex));
        errors.add(new CheckErrorResult(duplicateIndex, duplicateIndex + endIndex - beginIndex));
      }
      startIndex = endIndex;
    }
    return result;
  }
}
