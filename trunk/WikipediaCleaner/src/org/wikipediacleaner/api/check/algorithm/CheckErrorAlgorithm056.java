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
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 56 of check wikipedia project.
 * Error 56: Arrow as ASCII art
 */
public class CheckErrorAlgorithm056 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm056() {
    super("Arrow as ASCII art");
  }

  private final static String[][] allArrows = {
    { "<--->", "↔" },
    { "<-->" , "↔" },
    { "<->"  , "↔" },
    { "<–––>", "↔" },
    { "<––>" , "↔" },
    { "<–>"  , "↔" },
    { "<———>", "↔" },
    { "<——>" , "↔" },
    { "<—>"  , "↔" },

    { "<---" , "←" },
    { "<--"  , "←" },
    { "<-"   , "←" },
    { "<–––" , "←" },
    { "<––"  , "←" },
    { "<–"   , "←" },
    { "<———" , "←" },
    { "<——"  , "←" },
    { "<—"   , "←" },

    { "<===>", "⇔" },
    { "<==>" , "⇔" },
    { "<=>"  , "⇔" },

    { "<===" , "⇐" },
    { "<=="  , "⇐" },
    { "<="   , "⇐", "≤" },

    { "--->", "→" },
    { "-->" , "→" },
    { "->"  , "→" },
    { "–––>", "→" },
    { "––>" , "→" },
    { "–>"  , "→" },
    { "———>", "→" },
    { "——>" , "→" },
    { "—>"  , "→" },

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

    // Check each character from the beginning
    boolean result = false;
    int currentIndex = 0;
    String contents = pageAnalysis.getContents();
    while (currentIndex < contents.length()) {
      boolean shouldCheck = true;
      int nextIndex = currentIndex + 1;

      if (shouldCheck) {
        PageElementComment comment = pageAnalysis.isInComment(currentIndex);
        if (comment != null) {
          nextIndex = comment.getEndIndex();
          shouldCheck = false;
        }
      }
      if (shouldCheck) {
        PageElementTag tagNoWiki = pageAnalysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_NOWIKI, currentIndex);
        if (tagNoWiki != null) {
          nextIndex = tagNoWiki.getCompleteEndIndex();
          shouldCheck = false;
        }
      }
      if (shouldCheck) {
        PageElementTag tagSource = pageAnalysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_SOURCE, currentIndex);
        if (tagSource != null) {
          nextIndex = tagSource.getCompleteEndIndex();
          shouldCheck = false;
        }
      }
      if (shouldCheck) {
        PageElementTag tagHiero = pageAnalysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_HIERO, currentIndex);
        if (tagHiero != null) {
          nextIndex = tagHiero.getCompleteEndIndex();
          shouldCheck = false;
        }
      }
      if (shouldCheck) {
        // Check each kind of arrow
        int arrowLen = 0;
        String[] arrows = null;
        for (int i = 0; (i < allArrows.length) && (arrowLen == 0); i++) {
          if ((allArrows[i] != null) &&
              (allArrows[i].length > 0) &&
              (allArrows[i][0] != null) &&
              (contents.startsWith(allArrows[i][0], currentIndex))) {
            arrowLen = allArrows[i][0].length();
            arrows = allArrows[i];
          }
        }
  
        // Check if a possible arrow has been found
        if (arrowLen > 0) {
          if (arrows != null) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), currentIndex, currentIndex + arrowLen);
            for (int i = 1; i < arrows.length; i++) {
              errorResult.addReplacement(arrows[i]);
            }
            errors.add(errorResult);
          }
          nextIndex = currentIndex + arrowLen;
        }
      }
      currentIndex = nextIndex;
    }

    return result;
  }
}
