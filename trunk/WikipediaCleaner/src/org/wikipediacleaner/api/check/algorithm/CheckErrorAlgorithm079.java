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
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 79 of check wikipedia project.
 * Error 79: External link without description.
 */
public class CheckErrorAlgorithm079 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm079() {
    super("External link without description");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
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
      ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf(protocol, startIndex);
      if (startIndex >= 0) {
        int endIndex = contents.indexOf("]", startIndex);
        int spaceIndex = contents.indexOf(" ", startIndex);
        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          if ((spaceIndex < 0) || (spaceIndex > endIndex)) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(new CheckErrorResult(getShortDescription(), startIndex, endIndex + 1));
            startIndex = endIndex + 1;
          } else {
            startIndex = endIndex + 1;
          }
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
