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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementComment;


/**
 * Algorithm for analyzing error 92 of check wikipedia project.
 * Error 92: Headline double
 */
public class CheckErrorAlgorithm092 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm092() {
    super("Headline double");
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
    boolean result = false;
    int startIndex = 0;
    int previousTitleLevel = 0;
    HashMap<Integer, ArrayList<String>> titles = new HashMap<Integer, ArrayList<String>>();
    while (startIndex < contents.length()) {
      int titleIndex = contents.indexOf("=", startIndex);
      if (titleIndex < 0) {
        startIndex = contents.length();
      } else {
        int endLineIndex = contents.indexOf("\n", titleIndex);
        if (endLineIndex < 0) {
          endLineIndex = contents.length();
        }
        if ((titleIndex == 0) || (contents.charAt(titleIndex - 1) == '\n')) {
          // Title found
          int titleLevel = 0;
          int currentBegin = titleIndex;
          // Count number of '=' at the beginning to get title level
          while ((currentBegin < contents.length()) && (contents.charAt(currentBegin) == '=')) {
            currentBegin++;
            titleLevel++;
          }
          // Possible whitespaces
          while ((currentBegin < contents.length()) && (contents.charAt(currentBegin) == ' ')) {
            currentBegin++;
          }
          // Remove '=' at the end of the title
          int currentEnd = endLineIndex - 1;
          while ((currentEnd > currentBegin) && (contents.charAt(currentEnd) == '=')) {
            currentEnd--;
          }
          // Possible whitespaces
          while ((currentEnd > currentBegin) && (contents.charAt(currentEnd) == ' ')) {
            currentEnd--;
          }

          // Check title level
          if (titleLevel < previousTitleLevel) {
            for (int i = previousTitleLevel; i > titleLevel; i--) {
              titles.remove(Integer.valueOf(i));
            }
          } else {
            ArrayList<String> knownTitles = titles.get(Integer.valueOf(titleLevel));
            String title = contents.substring(currentBegin, currentEnd + 1);
            if (knownTitles == null) {
              knownTitles = new ArrayList<String>();
              knownTitles.add(title);
              titles.put(Integer.valueOf(titleLevel), knownTitles);
            } else if (!knownTitles.contains(title)) {
              knownTitles.add(title);
            } else {
              if (errors == null) {
                return true;
              }
              result = true;
              errors.add(createCheckErrorResult(page, titleIndex, endLineIndex));
            }
          }
          previousTitleLevel = titleLevel;
          startIndex = endLineIndex + 1;
        } else {
          startIndex = endLineIndex + 1;
        }
      }
    }
    return result;
  }
}
