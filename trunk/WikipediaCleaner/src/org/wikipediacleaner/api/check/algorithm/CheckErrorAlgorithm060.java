/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;


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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    for (PageElementTemplate template : analysis.getTemplates()) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        String paramValue = template.getParameterValue(paramNum);
        if (paramValue != null) {
          int squareBracketsCount = 0;
          int paramValueOffset = template.getParameterValueOffset(paramNum);
          for (int currentPos = 0; currentPos < paramValue.length(); currentPos++) {
            switch (paramValue.charAt(currentPos)) {
            case '<':
              int tmpIndex = paramValueOffset + currentPos;
              PageElementComment comment = analysis.isInComment(tmpIndex);
              if (comment != null) {
                currentPos = comment.getEndIndex() - 1 - paramValueOffset;
              } else {
                PageElementTag tag = analysis.isInTag(tmpIndex);
                if ((tag != null) &&
                    (tag.getBeginIndex() == tmpIndex) &&
                    ((PageElementTag.TAG_WIKI_MATH.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_SOURCE.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_SCORE.equals(tag.getNormalizedName())) ||
                     (PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT.equals(tag.getNormalizedName())))) {
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
                    analysis,
                    paramValueOffset + currentPos,
                    paramValueOffset + currentIndex);
                errorResult.addReplacement("");
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
