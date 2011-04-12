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


import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 64 of check wikipedia project.
 * Error 64: Link equal to linktext <br/>
 */
public class CheckErrorAlgorithm064 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm064() {
    super("Link equal to linktext");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    int beginIndex = -1;
    int endIndex = 0;
    int pipeIndex = 0;
    while (startIndex < contents.length()) {
      if (beginIndex < startIndex) {
        beginIndex = contents.indexOf("[[", startIndex);
      }
      if (beginIndex < 0) {
        return result;
      }
      if (endIndex <= beginIndex) {
        endIndex = contents.indexOf("]]", beginIndex);
      }
      if (endIndex < 0) {
        return result;
      }
      if (pipeIndex <= beginIndex) {
        pipeIndex = contents.indexOf("|", beginIndex);
      }
      if (pipeIndex < 0) {
        return result;
      }
      if (endIndex < pipeIndex) {
        startIndex = endIndex + 1;
      } else {
        String link = contents.substring(beginIndex + 2, pipeIndex);
        String text = contents.substring(pipeIndex + 1, endIndex);
        if (link.equals(text)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              page, beginIndex, endIndex + 2);
          errorResult.addReplacement("[[" + text + "]]");
          errors.add(errorResult);
        }
        startIndex = beginIndex + 1;
      }
    }
    return result;
  }
}
