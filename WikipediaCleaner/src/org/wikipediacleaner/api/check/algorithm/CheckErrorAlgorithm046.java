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
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 46 of check wikipedia project.
 * Error 46: Square brackets not correct begin
 */
public class CheckErrorAlgorithm046 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm046() {
    super("Square brackets not correct begin");
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

    // Analyze contents from the beginning by counting [[ and ]]
    int startIndex = 0;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    int beginIndex = contents.indexOf("[[", startIndex);
    int endIndex = contents.indexOf("]]", startIndex);
    int count = 0;
    while (startIndex < contents.length()) {
      if ((beginIndex < 0) && (endIndex < 0)) {
        // No more [[ or ]]
        startIndex = contents.length();
      } else if ((beginIndex >= 0) && ((beginIndex < endIndex) || (endIndex < 0))) {
        // Found a [[
        count++;
        startIndex = beginIndex + 2;
        beginIndex = contents.indexOf("[[", startIndex);
      } else {
        // Found a ]]
        count--;
        if (count < 0) {
          // Found more ]] than [[
          if (errors == null) {
            return true;
          }
          result = true;

          // Check if there is a potential beginning
          int tmpIndex = endIndex - 1;
          boolean errorReported = false;
          boolean finished = false;
          while ((!finished) && (tmpIndex >= 0)) {
            char tmpChar = contents.charAt(tmpIndex);
            if ((tmpChar == '\n') || (tmpChar == ']')) {
              finished = true;
            } else if (tmpChar == '[') {
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), tmpIndex, endIndex + 2);
              errorResult.addReplacement("[" + contents.substring(tmpIndex, endIndex + 2));

              // Check if the situation is something like [http://....]] (replacement: [http://....])
              List<String> protocols = PageElementExternalLink.getProtocols();
              boolean protocolFound = false;
              for (String protocol : protocols) {
                if (contents.startsWith(protocol, tmpIndex + 1)) {
                  protocolFound = true;
                }
              }
              if (protocolFound) {
                errorResult.addReplacement(contents.substring(tmpIndex, endIndex + 1));
              }

              errors.add(errorResult);
              errorReported = true;
              finished = true;
            } else if (tmpChar == '{') {
              int firstChar = tmpIndex;
              if ((firstChar > 0) && (contents.charAt(firstChar - 1) == '{')) {
                firstChar--;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), firstChar, endIndex + 2);
              errorResult.addReplacement("[[" + contents.substring(tmpIndex + 1, endIndex + 2));
              errorResult.addReplacement("{{" + contents.substring(tmpIndex + 1, endIndex) + "}}");
              errors.add(errorResult);
              errorReported = true;
              finished = true;
            }
            tmpIndex--;
          }

          // Default
          if (!errorReported) {
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), endIndex, endIndex + 2);
            errorResult.addReplacement("", GT._("Delete"));
            errors.add(errorResult);
          }
          count = 0;
        }
        startIndex = endIndex + 2;
        endIndex = contents.indexOf("]]", startIndex);
      }
    }
    return result;
  }
}
