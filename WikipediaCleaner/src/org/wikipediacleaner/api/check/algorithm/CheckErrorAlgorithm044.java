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
 * Algorithm for analyzing error 44 of check wikipedia project.
 * Error 44: Headlines with bold
 */
public class CheckErrorAlgorithm044 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm044() {
    super("Headlines with bold");
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

      // Find next =
      int titleIndex = contents.indexOf("=", startIndex);
      if (titleIndex < 0) {
        startIndex = contents.length();
      } else {

        // Check if the = is the beginning of a title
        int endLineIndex = contents.indexOf("\n", titleIndex);
        if ((titleIndex == 0) || (contents.charAt(titleIndex - 1) == '\n')) {
          int titleLevel = 0;
          int currentBegin = titleIndex;
          // Reading the = at the beginning of the title
          while ((currentBegin < contents.length()) && (contents.charAt(currentBegin) == '=')) {
            currentBegin++;
            titleLevel++;
          }
          if (endLineIndex < 0) {
            endLineIndex = contents.length();
          }
          // Reading possible whitespaces
          while ((currentBegin < contents.length()) && (contents.charAt(currentBegin) == ' ')) {
            currentBegin++;
          }

          int currentEnd = endLineIndex - 1;
          // Reading possible whitespaces
          while ((currentEnd > currentBegin) && (contents.charAt(currentEnd) == ' ')) {
            currentEnd--;
          }
          // Reading the = at the end of the title
          while ((currentEnd > currentBegin) && (contents.charAt(currentEnd) == '=')) {
            currentEnd--;
          }
          // Reading possible whitespaces
          while ((currentEnd > currentBegin) && (contents.charAt(currentEnd) == ' ')) {
            currentEnd--;
          }

          // Analyzing the title
          String text = contents.substring(titleIndex, currentBegin);
          int count = 0;
          while (currentBegin <= currentEnd) {
            if (contents.startsWith("'''", currentBegin)) {
              count++;
              currentBegin += 3;
            } else {
              text += contents.charAt(currentBegin);
              currentBegin++;
            }
          }
          if (count > 1) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), titleIndex, endLineIndex);
            errorResult.addReplacement(text + contents.substring(currentEnd + 1, endLineIndex));
            errors.add(errorResult);
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
