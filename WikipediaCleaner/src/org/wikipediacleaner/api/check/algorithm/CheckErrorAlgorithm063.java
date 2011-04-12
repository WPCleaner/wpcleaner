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
 * Algorithm for analyzing error 63 of check wikipedia project.
 * Error 63: HTML text style element &lt;small&gt; in ref, sub or sup
 */
public class CheckErrorAlgorithm063 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm063() {
    super("HTML text style element <small> in ref, sub or sup");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    int levelRef = 0;
    int levelSub = 0;
    int levelSup = 0;
    int levelSmall = 0;
    int errorLevel = -1;
    int errorIndex = -1;
    while (startIndex < contents.length()) {
      switch (contents.charAt(startIndex)) {
      case '<':
        if (contents.startsWith("<ref", startIndex)) {
          startIndex += 3;
          while ((startIndex < contents.length()) &&
                 (contents.charAt(startIndex) != '>')) {
            startIndex++;
          }
          if ((startIndex < contents.length()) &&
              (contents.charAt(startIndex) == '>')) {
            if (contents.charAt(startIndex - 1) != '/') {
              levelRef++;
            }
          }
        } else if (contents.startsWith("</ref", startIndex)) {
          levelRef--;
          startIndex += 4;
        } else if (contents.startsWith("<sub", startIndex)) {
          startIndex += 3;
          while ((startIndex < contents.length()) &&
                 (contents.charAt(startIndex) != '>')) {
            startIndex++;
          }
          if ((startIndex < contents.length()) &&
              (contents.charAt(startIndex) == '>')) {
            if (contents.charAt(startIndex - 1) != '/') {
              levelSub++;
            }
          }
        } else if (contents.startsWith("</sub", startIndex)) {
          levelSub--;
          startIndex += 4;
        } else if (contents.startsWith("<sup", startIndex)) {
          startIndex += 3;
          while ((startIndex < contents.length()) &&
                 (contents.charAt(startIndex) != '>')) {
            startIndex++;
          }
          if ((startIndex < contents.length()) &&
              (contents.charAt(startIndex) == '>')) {
            if (contents.charAt(startIndex - 1) != '/') {
              levelSup++;
            }
          }
        } else if (contents.startsWith("</sup", startIndex)) {
          levelSup--;
          startIndex += 4;
        } else if (contents.startsWith("<small>", startIndex)) {
          if ((levelRef > 0) || (levelSub > 0) || (levelSup > 0)) {
            if (errorLevel < 0) {
              errorLevel = levelSmall;
              errorIndex = startIndex;
            }
          }
          levelSmall++;
          startIndex += 6;
        } else if (contents.startsWith("</small>", startIndex)) {
          levelSmall--;
          if (levelSmall < 0) {
            levelSmall = 0;
          }
          startIndex += 7;
          if ((levelRef > 0) || (levelSub > 0) || (levelSup > 0)) {
            if (levelSmall == errorLevel) {
              if (errors == null) {
                return true;
              }
              result = true;
              errorLevel = -1;
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, errorIndex, startIndex + 1);
              errors.add(errorResult);
            }
          }
        }
        break;
      }
      startIndex++;
    }
    return result;
  }
}
