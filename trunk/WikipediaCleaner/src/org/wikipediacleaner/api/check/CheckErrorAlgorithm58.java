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
 * Algorithm for analyzing error 58 of check wikipedia project.
 * Error 58: Headline ALL CAPS
 */
public class CheckErrorAlgorithm58 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm58() {
    super("Headline ALL CAPS");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      int titleIndex = contents.indexOf("=", startIndex);
      if (titleIndex < 0) {
        startIndex = contents.length();
      } else {
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
          int lettersFound = 0;
          boolean upperCaseFound = false;
          boolean lowerCaseFound = false;
          while (currentBegin <= currentEnd) {
            if (Character.isLowerCase(contents.charAt(currentBegin))) {
              lettersFound++;
              lowerCaseFound = true;
            } else if (Character.isUpperCase(contents.charAt(currentBegin))) {
              lettersFound++;
              upperCaseFound = true;
            }
            currentBegin++;
          }
          if (upperCaseFound && !lowerCaseFound && (lettersFound >= 10)) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(new CheckErrorResult(getShortDescription(), titleIndex, endLineIndex));
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
