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
 * Algorithm for analyzing error 80 of check wikipedia project.
 * Error 80: External link with line break.
 */
public class CheckErrorAlgorithm080 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm080() {
    super("External link with line break");
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
    boolean result = false;
    result |= analyzeProtocol("[http://", pageAnalysis, errors);
    result |= analyzeProtocol("[ftp://", pageAnalysis, errors);
    result |= analyzeProtocol("[https://", pageAnalysis, errors);
    return result;
  }

  /**
   * Check for errors for on protocol.
   * 
   * @param protocol Protocol.
   * @param pageAnalysis Page analysis.
   * @param contents Page contents.
   * @return
   */
  private boolean analyzeProtocol(
      String protocol, PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf(protocol, startIndex);
      if (startIndex >= 0) {
        int endIndex = contents.indexOf("]", startIndex);
        int lineIndex = contents.indexOf("\n", startIndex);
        if (endIndex < 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          if (lineIndex < 0) {
            errors.add(createCheckErrorResult(
                pageAnalysis.getPage(), startIndex, contents.length()));
            startIndex = contents.length();
          } else {
            errors.add(createCheckErrorResult(
                pageAnalysis.getPage(), startIndex, lineIndex));
            startIndex = lineIndex + 1;
          }
        } else {
          if ((lineIndex >= 0) && (lineIndex < endIndex)) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(new CheckErrorResult(getShortDescription(), startIndex, lineIndex));
            startIndex = lineIndex + 1;
          } else {
            startIndex = endIndex;
          }
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
