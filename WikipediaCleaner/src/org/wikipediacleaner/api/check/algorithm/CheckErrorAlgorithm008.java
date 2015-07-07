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

    // Check every "=" at the beginning of a line
    boolean result = false;
    String contents = analysis.getContents();
    int maxLen = contents.length();
    for (int currentIndex = 0; currentIndex < maxLen; currentIndex++) {
      if ((contents.charAt(currentIndex) == '=') &&
          ((currentIndex == 0) || (contents.charAt(currentIndex - 1) == '\n'))) {

        // Check that it is indeed an error
        boolean errorFound = true;
        if (errorFound) {
          if (analysis.isInComment(currentIndex) != null) {
            errorFound = false;
          }
        }
        if (errorFound) {
          PageElementTitle title = analysis.isInTitle(currentIndex);
          if ((title != null)  && (title.getSecondLevel() >= title.getFirstLevel())) {
            errorFound = false;
          }
        }
        if (errorFound) {
          if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_CODE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
              (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null)) {
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
              analysis, currentIndex, endLineIndex);
          errorResult.addReplacement(PageElementTitle.createTitle(
              equalsCount,
              contents.substring(currentIndex + equalsCount, endLineIndex),
              null));
          if (equalIndex > 0) {
            String firstPart = contents.substring(currentIndex + equalsCount, equalIndex); 
            errorResult.addReplacement(PageElementTitle.createTitle(
                equalsCount, firstPart, null));
            while ((equalIndex < endLineIndex) && (contents.charAt(equalIndex) == '=')) {
              equalIndex++;
            }
            errorResult.addReplacement(
                PageElementTitle.createTitle(equalsCount, firstPart, null) + "\n" +
                contents.substring(equalIndex, endLineIndex));
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
