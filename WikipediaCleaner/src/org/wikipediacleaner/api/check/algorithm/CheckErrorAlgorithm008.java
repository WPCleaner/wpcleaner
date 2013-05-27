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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 8 of check wikipedia project.
 * Error 8: Headline should end with "="
 */
public class CheckErrorAlgorithm008 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm008() {
    super("Headline should end with \"=\"");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }

    // Check every "=" at the beginning of a line
    boolean result = false;
    String contents = analysis.getContents();
    int maxLen = contents.length();
    for (int currentIndex = 0; currentIndex < maxLen; currentIndex++) {
      if ((contents.charAt(currentIndex) == '=') &&
          ((currentIndex == 0) || (contents.charAt(currentIndex - 1) == '\n'))) {

        // Check that it is indeed an error
        boolean errorFound = true;
        if ((analysis.isInComment(currentIndex) != null) ||
            (analysis.isInTitle(currentIndex) != null)) {
          errorFound = false;
        } else {
          if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_CODE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, currentIndex) != null)) {
            errorFound = false;
          }
        }

        // Signal error
        if (errorFound) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Find end of a line and potential "=" sign
          int equalIndex = -1;
          int endLineIndex = currentIndex;
          int equalsCount = 0;
          while ((endLineIndex < maxLen) && (contents.charAt(endLineIndex) == '=')) {
            endLineIndex++;
            equalsCount++;
          }
          while ((endLineIndex < maxLen) && (contents.charAt(endLineIndex) != '\n')) {
            if ((equalIndex < 0) && (contents.charAt(endLineIndex) == '=')) {
              equalIndex = endLineIndex;
            }
            endLineIndex++;
          }

          // Create error
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), currentIndex, endLineIndex);
          errorResult.addReplacement(PageElementTitle.createTitle(
              equalsCount,
              contents.substring(currentIndex + equalsCount, endLineIndex)));
          if (equalIndex > 0) {
            String firstPart = contents.substring(currentIndex + equalsCount, equalIndex); 
            errorResult.addReplacement(PageElementTitle.createTitle(equalsCount, firstPart));
            while ((equalIndex < endLineIndex) && (contents.charAt(equalIndex) == '=')) {
              equalIndex++;
            }
            errorResult.addReplacement(
                PageElementTitle.createTitle(equalsCount, firstPart) + "\n" +
                contents.substring(equalIndex, endLineIndex));
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
