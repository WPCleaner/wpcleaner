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

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 67 of check wikipedia project.
 * Error 67: Reference after punctuation.
 */
public class CheckErrorAlgorithm067 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm067() {
    super("Reference after punctuation");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
    ArrayList<String> names = new ArrayList<String>();
    while (startIndex < contents.length()) {
      if (contents.charAt(startIndex) == '<') {
        int beginIndex = startIndex;
        int currentIndex = beginIndex + 1;
        if (contents.startsWith("ref", currentIndex)) {
          currentIndex += 3;
          String name = null;
          while ((currentIndex < contents.length()) &&
                 (contents.charAt(currentIndex) != '/') &&
                 (contents.charAt(currentIndex) != '>') &&
                 (contents.charAt(currentIndex) != '<')) {
            if (contents.startsWith(" name=", currentIndex)) {
              currentIndex += 6;
              char separator = contents.charAt(currentIndex);
              if ((separator == '"') || (separator == '\'')) {
                currentIndex++;
                int beginName = currentIndex;
                while ((currentIndex < contents.length()) &&
                       (contents.charAt(currentIndex) != separator) &&
                       (contents.charAt(currentIndex) != '/') &&
                       (contents.charAt(currentIndex) != '>') &&
                       (contents.charAt(currentIndex) != '<')) {
                  currentIndex++;
                }
                if (contents.charAt(currentIndex) == separator) {
                  name = contents.substring(beginName, currentIndex).trim();
                  if (!names.contains(name)) {
                    names.add(name);
                  }
                }
              }
            } else {
              currentIndex++;
            }
          }
          boolean simpleTag = false;
          if ((currentIndex < contents.length()) &&
              (contents.charAt(currentIndex) == '/')) {
            simpleTag = true;
            currentIndex++;
          }
          if ((currentIndex < contents.length()) &&
              (contents.charAt(currentIndex) == '>')) {
            currentIndex++;
            int beginRefIndex = currentIndex;
            int endRefIndex = simpleTag ? beginRefIndex : contents.indexOf("</ref>", beginRefIndex);
            if (endRefIndex < 0) {
              currentIndex = contents.length();
            } else {
              currentIndex = endRefIndex + (simpleTag ? 0 : 6);

              // Remove possible whitespaces
              int tmpIndex = beginIndex - 1;
              while ((tmpIndex >= 0) && (Character.isWhitespace(contents.charAt(tmpIndex)))) {
                tmpIndex--;
              }

              // Check if previous character is a punctuation
              char punctuation = contents.charAt(tmpIndex);
              if ((tmpIndex >= 0) && (SpecialCharacters.isPunctuation(punctuation))) {
                if (errors == null) {
                  return true;
                }
                result = true;
                int endIndex = currentIndex;
                while ((currentIndex < contents.length()) &&
                       (punctuation == contents.charAt(currentIndex))) {
                  currentIndex++;
                }
                CheckErrorResult errorResult = new CheckErrorResult(
                    getShortDescription(), tmpIndex, currentIndex);
                errorResult.addReplacement(
                    contents.substring(beginIndex, endIndex) + punctuation);
                if (currentIndex > endIndex) {
                  errorResult.addReplacement(
                      contents.substring(beginIndex, endIndex) + punctuation + contents.substring(endIndex, currentIndex));
                }
                errors.add(errorResult);
              }
            }
          }
        }
        startIndex = currentIndex;
      } else {
        startIndex++;
      }
    }
    return result;
  }
}
