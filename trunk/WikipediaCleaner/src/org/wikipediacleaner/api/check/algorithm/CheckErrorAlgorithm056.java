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
import org.wikipediacleaner.api.data.PageElementComment;


/**
 * Algorithm for analyzing error 56 of check wikipedia project.
 * Error 56: Arrow as ASCII art
 */
public class CheckErrorAlgorithm056 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm056() {
    super("Arrow as ASCII art");
  }

  private final static String[][] leftArrows = {
    { "<--->", "↔" },
    { "<-->" , "↔" },
    { "<->"  , "↔" },

    { "<---" , "←" },
    { "<--"  , "←" },
    { "<-"   , "←" },

    { "<===>", "⇔" },
    { "<==>" , "⇔" },
    { "<=>"  , "⇔" },

    { "<===" , "⇐" },
    { "<=="  , "⇐" },
    { "<="   , "⇐", "≤" },
  };
  private final static String[][] simpleRightArrows = {
    { "--->", "→" },
    { "-->" , "→" },
    { "->"  , "→" },
  };
  private final static String[][] doubleRightArrows = {
    { "===>", "⇒" },
    { "==>" , "⇒" },
    { "=>"  , "⇒", "≥" },
  };

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

    // Retrieve all comments
    List<PageElementComment> comments = pageAnalysis.getComments();
    int maxComments = comments.size();
    int currentComment = 0;

    // Check each character from the beginning
    boolean result = false;
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      // Check if in comment
      while ((currentComment < maxComments) &&
             (comments.get(currentComment).getEndIndex() <= startIndex)) {
        currentComment++;
      }
      if ((currentComment < maxComments) &&
          (comments.get(currentComment).getBeginIndex() <= startIndex)) {
        startIndex = comments.get(currentComment).getEndIndex();
      } else {
        int arrowLen = 0;
        String[] arrows = null;
        switch (contents.charAt(startIndex)) {
        case '<':
          // Check for bidirectional arrows or left arrows
          for (int i = 0; (i < leftArrows.length) && (arrowLen == 0); i++) {
            if ((leftArrows[i] != null) &&
                (leftArrows[i].length > 0) &&
                (leftArrows[i][0] != null) &&
                (contents.startsWith(leftArrows[i][0], startIndex))) {
              arrowLen = leftArrows[i][0].length();
              arrows = leftArrows[i];
            }
          }
          break;
        case '-':
          // Check for right simple arrows
          for (int i = 0; (i < simpleRightArrows.length) && (arrowLen == 0); i++) {
            if ((simpleRightArrows[i] != null) &&
                (simpleRightArrows[i].length > 0) &&
                (simpleRightArrows[i][0] != null) &&
                (contents.startsWith(simpleRightArrows[i][0], startIndex))) {
              arrowLen = simpleRightArrows[i][0].length();
              arrows = simpleRightArrows[i];
            }
          }
          break;
        case '=':
          // Check for right double arrows
          for (int i = 0; (i < doubleRightArrows.length) && (arrowLen == 0); i++) {
            if ((doubleRightArrows[i] != null) &&
                (doubleRightArrows[i].length > 0) &&
                (doubleRightArrows[i][0] != null) &&
                (contents.startsWith(doubleRightArrows[i][0], startIndex))) {
              arrowLen = doubleRightArrows[i][0].length();
              arrows = doubleRightArrows[i];
            }
          }
          break;
        }

        // Check if a possible arrow has been found
        if (arrowLen > 0) {
          if (arrows != null) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), startIndex, startIndex + arrowLen);
            for (int i = 1; i < arrows.length; i++) {
              errorResult.addReplacement(arrows[i]);
            }
            errors.add(errorResult);
          }
          startIndex += arrowLen;
        } else {
          startIndex++;
        }
      }
    }
    return result;
  }
}
