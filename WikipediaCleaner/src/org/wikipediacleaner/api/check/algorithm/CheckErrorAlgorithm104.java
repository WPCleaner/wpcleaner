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
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 104 of check wikipedia project.
 * Error 104: Unbalanced quotes in ref name
 */
public class CheckErrorAlgorithm104 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm104() {
    super("Unbalanced quotes in ref name");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check every "<"
    boolean result = false;
    String contents = analysis.getContents();
    int maxLen = contents.length();
    int currentIndex = 0;
    while (currentIndex < maxLen) {
      int nextIndex = currentIndex + 1;
      boolean shouldReport = false;
      if (contents.charAt(currentIndex) == '<') {
        shouldReport = true;
      }
      if (shouldReport) {
        // Ignore tags correctly detected
        PageElementTag tag = analysis.isInTag(currentIndex);
        if ((tag != null) && (tag.getBeginIndex() == currentIndex)) {
          shouldReport = false;
        }
      }
      if (shouldReport) {
        // Ignore comments
        if (analysis.isInComment(currentIndex) != null) {
          shouldReport = false;
        }
      }
      if (shouldReport) {
        // Ignore some tags
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_CODE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null)) {
          shouldReport = false;
        }
      }
      String replacement = null;
      if (shouldReport) {
        int endIndex = currentIndex + 1;
        while ((endIndex < maxLen) &&
            Character.isWhitespace(contents.charAt(endIndex))) {
          endIndex++;
        }
        if ((endIndex >= maxLen) || !contents.startsWith("ref", endIndex)) {
          shouldReport = false;
        } else {
          endIndex += 3;
        }
        if ((endIndex >= maxLen) || !Character.isWhitespace(contents.charAt(endIndex))) {
          shouldReport = false;
        } else {
          while ((endIndex < maxLen) && Character.isWhitespace(contents.charAt(endIndex))) {
            endIndex++;
          }
        }
        if ((endIndex >= maxLen) || !contents.startsWith("name", endIndex)) {
          shouldReport = false;
        } else {
          endIndex += 4;
        }
        if (shouldReport) {
          nextIndex = endIndex;
          int tmpIndex = endIndex;
          int lastNonSpace = tmpIndex;
          if ((tmpIndex < maxLen) && (contents.charAt(tmpIndex) == '\"')) {
            tmpIndex++;
            while ((tmpIndex < maxLen) &&
                ((Character.isLetter(contents.charAt(tmpIndex))) ||
                 (Character.isDigit(contents.charAt(tmpIndex))) ||
                 (contents.charAt(tmpIndex) == ' '))) {
              tmpIndex++;
              if (contents.charAt(tmpIndex) != ' ') {
                lastNonSpace = tmpIndex;
              }
            }
          }
          if ((tmpIndex < maxLen) &&
              (lastNonSpace > endIndex) &&
              (contents.charAt(tmpIndex) == '>')) {
            endIndex = tmpIndex + 1;
            replacement =
                contents.substring(currentIndex, lastNonSpace + 1) +
                '\"' +
                contents.substring(lastNonSpace + 1, endIndex);
          }
        }
      }

      // Report error
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, currentIndex, nextIndex);
        if (replacement != null) {
          errorResult.addReplacement(replacement);
        }
        errors.add(errorResult);
      }
      currentIndex = nextIndex;
    }

    return result;
  }
}
