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

package org.wikipediacleaner.api.check;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 80 of check wikipedia project.
 * Error 80: External link with line break.
 */
public class CheckErrorAlgorithm80 extends CheckErrorAlgorithmBase {

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#getErrorDescription()
   */
  public String getErrorDescription() {
    return GT._("External link with line break");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page)
   */
  public boolean analyze(Page page) {
    boolean result = false;
    result |= analyzeProtocol("[http://", page);
    result |= analyzeProtocol("[ftp://", page);
    result |= analyzeProtocol("[https://", page);
    return result;
  }

  private boolean analyzeProtocol(String protocol, Page page) {
    boolean result = false;
    if ((page != null) && (page.getContents() != null)) {
      String contents = page.getContents();
      int currentIndex = 0;
      while ((!result) && (currentIndex < contents.length())) {
        int startIndex = contents.indexOf(protocol, currentIndex);
        if (startIndex < 0) {
          currentIndex = contents.length();
        } else {
          int endIndex = contents.indexOf("]", startIndex);
          if ((endIndex < 0)) {
            currentIndex = contents.length();
            result = true;
          } else {
            currentIndex = endIndex + 1;
            int lineIndex = contents.indexOf("\n", startIndex);
            if ((lineIndex >= 0) && (lineIndex < endIndex)) {
              result = true;
            }
          }
        }
      }
    }
    return result;
  }
}
