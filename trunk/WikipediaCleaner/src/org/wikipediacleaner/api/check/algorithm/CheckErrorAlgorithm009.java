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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;


/**
 * Algorithm for analyzing error 09 of check wikipedia project.
 * Error 09: Categories more at one line
 */
public class CheckErrorAlgorithm009 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm009() {
    super("Categories more at one line");
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

    int startIndex = 0;
    boolean result = false;
    Namespace categoryNamespace = Namespace.getNamespace(
        Namespace.CATEGORY, pageAnalysis.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return result;
    }
    boolean newLine = true;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      if (contents.charAt(startIndex) == '\n') {
        newLine = true;
        startIndex++;
      } else if (contents.startsWith("[[", startIndex)) {
        int beginIndex = startIndex;
        int currentIndex = beginIndex + 2;

        // Namespace
        int linkIndex = currentIndex;
        while ((currentIndex < contents.length()) &&
               (contents.charAt(currentIndex) != ':') &&
               (contents.charAt(currentIndex) != '|') &&
               (contents.charAt(currentIndex) != ']') &&
               (contents.charAt(currentIndex) != '[')) {
          currentIndex++;
        }

        // Check if namespace is Category
        if ((currentIndex < contents.length()) &&
            (contents.charAt(currentIndex) == ':') &&
            (categoryNamespace.isPossibleName(contents.substring(linkIndex, currentIndex).trim()))) {

          // Link itself
          currentIndex++;
          linkIndex = currentIndex;
          while ((currentIndex < contents.length()) &&
                 (contents.charAt(currentIndex) != '|') &&
                 (contents.charAt(currentIndex) != ']')) {
            currentIndex++;
          }

          // Go to the end
          while ((currentIndex < contents.length()) &&
                 (!contents.startsWith("]]", currentIndex))) {
            currentIndex++;
          }
          if ((currentIndex < contents.length()) &&
              (contents.startsWith("]]", currentIndex))) {
            currentIndex += 2;
            if (newLine == true) {
              newLine = false;
            } else {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), beginIndex, currentIndex);
              errorResult.addReplacement(
                  "\n[[" + categoryNamespace.getTitle() + ":" +
                  contents.substring(linkIndex, currentIndex));
              errors.add(errorResult);
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
