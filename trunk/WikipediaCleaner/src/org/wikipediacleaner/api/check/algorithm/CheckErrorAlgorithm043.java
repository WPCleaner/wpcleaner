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
import org.wikipediacleaner.api.data.PageElementComment;

/**
 * Algorithm for analyzing error 43 of check wikipedia project.
 * Error 43: Template not correct end
 */
public class CheckErrorAlgorithm043 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm043() {
    super("Template not correct end");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments in the page contents.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyze contents from the end by counting }} and {{
    int startIndex = contents.length();
    boolean result = false;
    int beginIndex = contents.lastIndexOf("{{", startIndex);
    int endIndex = contents.lastIndexOf("}}", startIndex);
    int count = 0;
    while (startIndex > 0) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        // No more }} or {{
        startIndex = 0;
      } else if ((endIndex >= 0) && ((beginIndex < endIndex) || (beginIndex < 0))) {
        // Found a }}
        count++;
        startIndex = endIndex;
        endIndex = contents.lastIndexOf("}}", startIndex - 1);
      } else {
        // Found a {{
        count--;
        if (count < 0) {
          // Found more {{ than }}
          if (errors == null) {
            return true;
          }
          result = true;

          // Check if the situation is something like {{....} (replacement: {{....}})
          boolean errorReported = false;
          int nextEnd = contents.indexOf('}', beginIndex + 2);
          if (nextEnd > 0) {
            int nextCR = contents.indexOf('\n', beginIndex + 2);
            int nextBegin = contents.indexOf('{', beginIndex + 2);
            if (((nextCR < 0) || (nextCR > nextEnd)) &&
                ((nextBegin < 0) || (nextBegin > nextEnd))) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, beginIndex, nextEnd + 1);
              errorResult.addReplacement(contents.substring(beginIndex, nextEnd + 1) + "}");
              errors.add(errorResult);
              errorReported = true;
            }
          }

          // Default
          if (!errorReported) {
            errors.add(createCheckErrorResult(page, beginIndex, beginIndex + 2));
          }
          count = 0;
        }
        startIndex = beginIndex;
        beginIndex = contents.lastIndexOf("{{", startIndex - 1);
      }
    }
    return result;
  }
}
