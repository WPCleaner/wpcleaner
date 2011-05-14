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
 * Algorithm for analyzing error 55 of check wikipedia project.
 * Error 55: HTML text style element &lt;small&gt; double
 */
public class CheckErrorAlgorithm055 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm055() {
    super("HTML text style element <small> double");
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

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {

      // Searching for next <small>
      int beginIndex = contents.indexOf("<small>", startIndex);
      if (beginIndex < 0) {
        startIndex = contents.length();
      } else {
        int currentPos = beginIndex + 7;
        int levelSmall = 1;
        int smallPos = currentPos;
        while ((currentPos < contents.length()) && (levelSmall > 0)) {
          if (contents.charAt(currentPos) == '<') {
            if (contents.startsWith("<small>", currentPos)) {
              if (levelSmall == 1) {
                smallPos = currentPos;
              }
              levelSmall++;
              currentPos += 6;
            } else if (contents.startsWith("</small>", currentPos)) {
              levelSmall--;
              if (levelSmall == 1) {
                if (errors == null) {
                  return true;
                }
                result = true;
                CheckErrorResult errorResult = createCheckErrorResult(
                    pageAnalysis.getPage(), smallPos, currentPos + 8);
                errors.add(errorResult);
              }
              currentPos += 7;
            }
          }
          currentPos++;
        }
        startIndex = currentPos;
      }
    }
    return result;
  }
}
