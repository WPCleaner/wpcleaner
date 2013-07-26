/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 60 of check wikipedia project.
 * Error 60: Template parameter with problem
 */
public class CheckErrorAlgorithm060 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm060() {
    super("Template parameter with problem");
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

    // Analyzing the text from the beginning
    boolean result = false;
    for (PageElementTemplate template : pageAnalysis.getTemplates()) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        String paramValue = template.getParameterValue(paramNum);
        if (paramValue != null) {
          int squareBracketsCount = 0;
          int paramValueOffset = template.getParameterValueOffset(paramNum);
          for (int currentPos = 0; currentPos < paramValue.length(); currentPos++) {
            switch (paramValue.charAt(currentPos)) {
            case '<':
              int tmpIndex = paramValueOffset + currentPos;
              PageElementComment comment = pageAnalysis.isInComment(tmpIndex);
              if (comment != null) {
                currentPos = comment.getEndIndex() - 1 - paramValueOffset;
              } else {
                PageElementTag tag = pageAnalysis.isInTag(tmpIndex);
                if ((tag != null) &&
                    (tag.getBeginIndex() == tmpIndex) &&
                    ((PageElementTag.TAG_WIKI_MATH.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_SOURCE.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_SCORE.equals(tag.getNormalizedName())))) {
                  currentPos = tag.getCompleteEndIndex() - 1 - paramValueOffset;
                }
              }
              break;
            case '[':
              squareBracketsCount++;
              break;
            case ']':
              if (squareBracketsCount > 0) {
                squareBracketsCount--;
              } else {
                if (errors == null) {
                  return true;
                }
                result = true;
                int currentIndex = currentPos;
                while ((currentIndex < paramValue.length()) &&
                       (paramValue.charAt(currentIndex) == ']')) {
                  currentIndex++;
                }
                CheckErrorResult errorResult = createCheckErrorResult(
                    pageAnalysis.getPage(),
                    paramValueOffset + currentPos,
                    paramValueOffset + currentIndex);
                errorResult.addReplacement("", GT._("Remove"));
                errors.add(errorResult);
              }
            }
          }
        }
      }
    }

    return result;
  }
}
