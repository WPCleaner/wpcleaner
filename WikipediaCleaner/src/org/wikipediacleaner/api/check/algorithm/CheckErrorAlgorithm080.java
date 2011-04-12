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
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 80 of check wikipedia project.
 * Error 80: External link with line break.
 */
public class CheckErrorAlgorithm080 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm080() {
    super("External link with line break");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, Collection<CheckErrorResult> errors) {
    boolean result = false;
    result |= analyzeProtocol("[http://", page, contents, errors);
    result |= analyzeProtocol("[ftp://", page, contents, errors);
    result |= analyzeProtocol("[https://", page, contents, errors);
    return result;
  }

  /**
   * Check for errors for on protocol.
   * 
   * @param protocol Protocol.
   * @param page Page.
   * @param contents Page contents.
   * @return
   */
  private boolean analyzeProtocol(
      String protocol, Page page, String contents,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
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
            errors.add(createCheckErrorResult(page, startIndex, contents.length()));
            startIndex = contents.length();
          } else {
            errors.add(createCheckErrorResult(page, startIndex, lineIndex));
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
