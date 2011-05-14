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
import org.wikipediacleaner.api.data.PageAnalysis;


/**
 * Algorithm for analyzing error 10 of check wikipedia project.
 * Error 10: Square brackets not correct end
 */
public class CheckErrorAlgorithm010 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm010() {
    super("Square brackets not correct end");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Analyze contents from the end by counting ]] and [[
    String contents = pageAnalysis.getContents();
    int startIndex = contents.length();
    boolean result = false;
    int beginIndex = contents.lastIndexOf("[[", startIndex);
    int endIndex = contents.lastIndexOf("]]", startIndex);
    int count = 0;
    while (startIndex > 0) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        // No more ]] or [[
        startIndex = 0;
      } else if ((endIndex >= 0) && ((beginIndex < endIndex) || (beginIndex < 0))) {
        // Found a ]]
        count++;
        startIndex = endIndex;
        endIndex = contents.lastIndexOf("]]", startIndex - 2);
      } else {
        // Found a [[
        count--;
        if (count < 0) {
          // Found more [[ than ]]
          if (errors == null) {
            return true;
          }
          result = true;

          // Check if the situation is something like [[....] (replacement: [[....]])
          boolean errorReported = false;
          int nextEnd = contents.indexOf(']', beginIndex + 2);
          if (nextEnd > 0) {
            int nextCR = contents.indexOf('\n', beginIndex + 2);
            int nextBegin = contents.indexOf('[', beginIndex + 2);
            if (((nextCR < 0) || (nextCR > nextEnd)) &&
                ((nextBegin < 0) || (nextBegin > nextEnd))) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), beginIndex, nextEnd + 1);
              errorResult.addReplacement(contents.substring(beginIndex, nextEnd + 1) + "]");

              // Check if the situation is something like [[http://....] (replacement: [http://....]) 
              if ((contents.startsWith("http://", beginIndex + 2)) ||
                  (contents.startsWith("https://", beginIndex + 2))) {
                errorResult.addReplacement(contents.substring(beginIndex + 1, nextEnd + 1));
              }

              errors.add(errorResult);
              errorReported = true;
            }
          }

          // Default
          if (!errorReported) {
            errors.add(createCheckErrorResult(
                pageAnalysis.getPage(), beginIndex, beginIndex + 2));
          }
          count = 0;
        }
        startIndex = beginIndex;
        beginIndex = contents.lastIndexOf("[[", startIndex - 2);
      }
    }
    return result;
  }
}
