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
 * Algorithm for analyzing error 19 of check wikipedia project.
 * Error 19: Headlines start with one "="
 */
public class CheckErrorAlgorithm019 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm019() {
    super("Headlines start with one \"=\"");
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
    boolean result = false;
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      int titleIndex = contents.indexOf("=", startIndex);
      if (titleIndex < 0) {
        startIndex = contents.length();
      } else {
        int endLineIndex = contents.indexOf("\n", titleIndex);
        if ((titleIndex == 0) || (contents.charAt(titleIndex - 1) == '\n')) {
          int titleLevel = 0;
          int currentPos = titleIndex;
          while ((currentPos < contents.length()) && (contents.charAt(currentPos) == '=')) {
            currentPos++;
            titleLevel++;
          }
          if (endLineIndex < 0) {
            endLineIndex = contents.length();
          }
          if (titleLevel < 2) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(createCheckErrorResult(
                pageAnalysis.getPage(), titleIndex, endLineIndex));
          }
          startIndex = endLineIndex + 1;
        } else {
          if (endLineIndex < 0) {
            startIndex = contents.length();
          } else {
            startIndex = endLineIndex;
          }
        }
      }
    }
    return result;
  }
}
