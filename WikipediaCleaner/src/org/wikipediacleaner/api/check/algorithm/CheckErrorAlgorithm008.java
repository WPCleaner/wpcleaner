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

import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;

/**
 * Algorithm for analyzing error 8 of check wikipedia project.
 * Error 8: Headline should end with "="
 */
public class CheckErrorAlgorithm008 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm008() {
    super("Headline should end with \"=\"");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
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
          // Title found
          int titleLevel = 0;
          int currentPos = titleIndex;
          // Count number of '=' at the beginning of title to know title level
          while ((currentPos < contents.length()) && (contents.charAt(currentPos) == '=')) {
            currentPos++;
            titleLevel++;
          }
          int titleLevelInitial = titleLevel;
          if (endLineIndex < 0) {
            endLineIndex = contents.length();
          }
          currentPos = endLineIndex - 1;
          // Possible whitespaces at the end
          while ((currentPos >= 0) && (Character.isWhitespace(contents.charAt(currentPos)))) {
            currentPos--;
          }
          // Counting '=' at the end of title
          while ((currentPos >= 0) && (contents.charAt(currentPos) == '=')) {
            currentPos--;
            titleLevel--;
          }
          if (titleLevel > 0) {
            if (errors == null) {
              return true;
            }
            result = true;

            // Report detailed result
            CheckErrorResult errorResult = createCheckErrorResult(page, titleIndex, endLineIndex);

            // Replacement : truncate if there's text after end title
            for (int pos = titleIndex + titleLevelInitial; pos < endLineIndex - titleLevelInitial; pos++) {
              if (contents.charAt(pos) == '=') {
                boolean allTitle = true;
                for (int i = 0; i < titleLevelInitial; i++) {
                  if (contents.charAt(pos + i) != '=') {
                    allTitle = false;
                  }
                }
                if (allTitle) {
                  errorResult.addReplacement(contents.substring(titleIndex, pos + titleLevelInitial));
                }
              }
            }

            // Replacement : complete line
            String replacement = contents.substring(titleIndex, currentPos + 1);
            for (int i = 0; i < titleLevel; i++) {
              replacement += "=";
            }
            replacement += contents.substring(currentPos + 1, endLineIndex);
            errorResult.addReplacement(replacement);

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
