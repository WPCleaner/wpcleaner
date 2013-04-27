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
 * Algorithm for analyzing error 75 of check wikipedia project.
 * Error 75: Indented list
 */
public class CheckErrorAlgorithm075 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm075() {
    super("Indented list");
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
    int startIndex = -1;
    String contents = pageAnalysis.getContents();
    String previousLine = "";
    String incorrectLine = null;
    while (startIndex < contents.length()) {
      if (startIndex < 0) {
        startIndex = 0;
      } else {
        startIndex = contents.indexOf('\n', startIndex);
        if (startIndex >= 0) {
          startIndex++;
        }
      }
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {
        if (contents.charAt(startIndex) == ':') {
          int endIndex = startIndex;
          while ((startIndex < contents.length()) &&
                 (":-*#".indexOf(contents.charAt(endIndex)) >= 0)) {
            endIndex++;
          }
          String newLine = contents.substring(startIndex, endIndex);
          int newLength = newLine.length();
          if ((newLength > 1) &&
              ((newLength > previousLine.length() + 1)) ||
               (!previousLine.startsWith(newLine.substring(0, newLength - 1)))) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), startIndex, endIndex);
            char lastChar = newLine.charAt(newLine.length() - 1);
            if (incorrectLine == null) {
              incorrectLine = newLine;
            }
            if (newLine.equals(incorrectLine)) {
              if (newLine.length() > previousLine.length() + 1) {
                errorResult.addReplacement(previousLine + lastChar);
              } else {
                errorResult.addReplacement(previousLine.substring(0, newLength - 1) + lastChar);
              }
            }
            errors.add(errorResult);
          } else {
            previousLine = newLine;
            incorrectLine = null;
          }
        } else {
          previousLine = "";
        }
      }
    }
    return result;
  }
}
