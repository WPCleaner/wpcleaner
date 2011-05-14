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
import java.util.HashMap;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.PageAnalysis;


/**
 * Algorithm for analyzing error 45 of check wikipedia project.
 * Error 45: Interwiki double
 */
public class CheckErrorAlgorithm045 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm045() {
    super("Interwiki double");
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
    HashMap<String, InterwikiElement> interwikis = new HashMap<String, InterwikiElement>();
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      if (contents.startsWith("[[", startIndex)) {
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

        // Retrieve namespace
        if ((currentIndex < contents.length()) &&
            (contents.charAt(currentIndex) == ':')) {

          // Link itself
          String namespace = contents.substring(linkIndex, currentIndex);
          currentIndex++;
          for (Language lg : pageAnalysis.getWikipedia().getLanguages()) {
            if (namespace.equals(lg.getCode())) {
              while ((currentIndex < contents.length()) &&
                     (contents.charAt(currentIndex) != ']')) {
                currentIndex++;
              }

              if ((currentIndex < contents.length()) &&
                  (contents.startsWith("]]", currentIndex))) {
                currentIndex += 2;
                InterwikiElement interwikiElement = interwikis.get(namespace);
                if (interwikiElement == null) {
                  interwikiElement = new InterwikiElement(namespace, beginIndex, currentIndex);
                  interwikis.put(namespace, interwikiElement);
                } else {
                  if (errors == null) {
                    return true;
                  }
                  result = true;
                  if (interwikiElement.errorResult == null) {
                    interwikiElement.errorResult = createCheckErrorResult(
                        pageAnalysis.getPage(),
                        interwikiElement.begin, interwikiElement.end,
                        CheckErrorResult.ErrorLevel.CORRECT);
                    errors.add(interwikiElement.errorResult);
                  }
                  CheckErrorResult errorResult = createCheckErrorResult(
                      pageAnalysis.getPage(), beginIndex, currentIndex);
                  errorResult.addReplacement("");
                  errors.add(errorResult);
                }
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

  /**
   * Class to hold information about an interwiki element.
   */
  static class InterwikiElement {
    String name;
    int begin;
    int end;
    CheckErrorResult errorResult;

    /**
     * @param name Interwiki name.
     * @param begin Begin position.
     * @param end End position.
     */
    public InterwikiElement(String name, int begin, int end) {
      this.name = name;
      this.begin = begin;
      this.end = end;
    }
  }
}
