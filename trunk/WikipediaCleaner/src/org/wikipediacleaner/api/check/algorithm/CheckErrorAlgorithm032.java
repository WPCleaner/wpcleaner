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

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementComment;

/**
 * Algorithm for analyzing error 32 of check wikipedia project.
 * Error 32: Double pipe in one link.
 */
public class CheckErrorAlgorithm032 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm032() {
    super("Double pipe in one link");
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
    int startIndex = 0;
    boolean result = false;
    Namespace fileNamespace = Namespace.getNamespace(Namespace.IMAGE, page.getWikipedia().getNamespaces());
    while (startIndex < contents.length()) {

      // Searching for next [[
      int beginIndex = contents.indexOf("[[", startIndex);
      if (beginIndex < 0) {
        startIndex = contents.length();
      } else {
        int levelSquareBrackets = 1;
        int levelCurlyBrackets = 0;
        int pipeFound = 0;
        ArrayList<Integer> pipeIndex = new ArrayList<Integer>();
        int currentPos = beginIndex + 2;
        while ((currentPos < contents.length()) && (levelSquareBrackets > 0)) {
          switch (contents.charAt(currentPos)) {
          case '[':
            // Checking if we have inside [[
            if ((currentPos + 1 < contents.length()) &&
                (contents.charAt(currentPos + 1) == '[')) {
              levelSquareBrackets++;
              currentPos++;
            }
            break;
          case ']':
            // Checking if we have ]]
            if ((currentPos + 1 < contents.length()) &&
                (contents.charAt(currentPos + 1) == ']')) {
              levelSquareBrackets--;
              currentPos++;
            }
            break;
          case '{':
            // Checking if we have {{
            if ((currentPos + 1 < contents.length()) &&
                (contents.charAt(currentPos + 1) == '{')) {
              levelCurlyBrackets++;
              currentPos++;
            }
            break;
          case '}':
            // Checking if we have }}
            if ((currentPos + 1 < contents.length()) &&
                (contents.charAt(currentPos + 1) == '}')) {
              levelCurlyBrackets--;
              currentPos++;
            }
            break;
          case ':':
            // Checking if we have a namespace
            if ((pipeFound == 0) &&
                (levelSquareBrackets == 1) &&
                (levelCurlyBrackets == 0) &&
                (fileNamespace != null) &&
                (fileNamespace.isPossibleName(contents.substring(beginIndex + 2, currentPos).trim()))) {
              // Shortcut to end the test
              levelSquareBrackets = 0;
            }
            break;
          case '|':
            // Checking if the | is counting for
            if ((levelSquareBrackets == 1) &&
                (levelCurlyBrackets == 0)) {
              pipeIndex.add(Integer.valueOf(currentPos));
              pipeFound++;
            }
            break;
          }
          currentPos++;
        }

        // Testing if the error has been found
        if ((levelSquareBrackets == 0) && (pipeFound > 1)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(page, beginIndex, currentPos);
          for (int i = 0; i < pipeIndex.size(); i++) {
            int beginText = pipeIndex.get(i).intValue();
            int endText = ((i + 1 < pipeIndex.size()) ? pipeIndex.get(i + 1).intValue() : currentPos - 2);
            if (contents.substring(beginText + 1, endText).trim().length() > 0) {
              errorResult.addReplacement(
                  contents.substring(beginIndex, pipeIndex.get(0).intValue()) +
                  contents.substring(beginText, endText) +
                  "]]");
            }
          }
          errors.add(errorResult);
        }
        startIndex = currentPos;
      }
    }
    return result;
  }
}
