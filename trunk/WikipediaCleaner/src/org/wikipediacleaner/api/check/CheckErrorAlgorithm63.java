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

import java.util.ArrayList;

import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 63 of check wikipedia project.
 * Error 63: HTML text style element &lt;small&gt; in ref, sub or sup
 */
public class CheckErrorAlgorithm63 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm63() {
    super("HTML text style element <small> in ref, sub or sup");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    int beginIndex = -1;
    int endIndex = -1;
    int beginRef = 0;
    int endRef = 0;
    int beginSub = 0;
    int endSub = 0;
    int beginSup = 0;
    int endSup = 0;
    int level = 0;
    int levelRef = 0;
    int levelSub = 0;
    int levelSup = 0;
    while (startIndex < contents.length()) {
      // Update position of next <small> and </small>
      if (beginIndex < startIndex) {
        beginIndex = contents.indexOf("<small>", startIndex);
      }
      if (endIndex < startIndex) {
        endIndex = contents.indexOf("</small>", startIndex);
      }

      if ((beginIndex < 0) || (endIndex < 0)) {
        // No more <small> or </small>
        startIndex = contents.length();
      } else {
        int firstIndex = Math.min(beginIndex, endIndex);

        // Update position of next <ref> and </ref>
        if ((beginRef >= 0) && (beginRef <= startIndex)) {
          beginRef = contents.indexOf("<ref>", startIndex);
        }
        if (beginRef >= 0) {
          firstIndex = Math.min(firstIndex, beginRef);
        }
        if ((endRef >= 0) && (endRef <= startIndex)) {
          endRef = contents.indexOf("</ref>", startIndex);
        }
        if (endRef >= 0) {
          firstIndex = Math.min(firstIndex, endRef);
        }

        // Update position of next <sub> and </sub>
        if ((beginSub >= 0) && (beginSub <= startIndex)) {
          beginSub = contents.indexOf("<sub>", startIndex);
        }
        if (beginSub >= 0) {
          firstIndex = Math.min(firstIndex, beginSub);
        }
        if ((endSub >= 0) && (endSub <= startIndex)) {
          endSub = contents.indexOf("</sub>", startIndex);
        }
        if (endSub >= 0) {
          firstIndex = Math.min(firstIndex, endSub);
        }

        // Update position of next <sup> and </sup>
        if ((beginSup >= 0) && (beginSup <= startIndex)) {
          beginSup = contents.indexOf("<sup>", startIndex);
        }
        if (beginSup >= 0) {
          firstIndex = Math.min(firstIndex, beginSup);
        }
        if ((endSup >= 0) && (endSup <= startIndex)) {
          endSup = contents.indexOf("</sup>", startIndex);
        }
        if (endSup >= 0) {
          firstIndex = Math.min(firstIndex, endSup);
        }

        if (beginRef == firstIndex) {
          // Next element is <ref>
          levelRef++;
          startIndex = beginRef + 1;
        } else if (endRef == firstIndex) {
          // Next element is </ref>
          levelRef--;
          startIndex = endRef + 1;
        } else if (beginSub == firstIndex) {
          // Next element is <sub>
          levelSub++;
          startIndex = beginSub + 1;
        } else if (endSub == firstIndex) {
          // Next element is </sub>
          levelSub--;
          startIndex = endSub + 1;
        } else if (beginSup == firstIndex) {
          // Next element is <sup>
          levelSup++;
          startIndex = beginSup + 1;
        } else if (endSup == firstIndex) {
          // Next element is </sup>
          levelSup--;
          startIndex = endSup + 1;
        } else if (beginIndex == firstIndex) {
          // Next element is <small>
          level++;
          if ((level > 0) &&
              ((levelRef > 0) || (levelSub > 0) || (levelSup > 0))) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(new CheckErrorResult(getShortDescription(), beginIndex, endIndex + "</small>".length()));
          }
          startIndex = beginIndex + 1;
        } else {
          // Next element is </small>
          level--;
          startIndex = endIndex + 1;
        }
      }
    }
    return result;
  }
}
