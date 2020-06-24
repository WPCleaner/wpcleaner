/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 551 of check wikipedia project.
 * Error 551: Empty line.
 */
public class CheckErrorAlgorithm551 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm551() {
    super("Empty line");
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

    // Check each line
    String contents = analysis.getContents();
    int currentIndex = 0;
    boolean result = false;
    while (currentIndex < contents.length()) {
      int beginLine = currentIndex;
      boolean emptyLine = true;
      boolean something = false;
      int countItalic = 0;
      int countBold = 0;
      int endTag = beginLine;
      boolean unclosedTags = false;
      while (emptyLine && (currentIndex < contents.length())) {
        char currentChar = contents.charAt(currentIndex);
        if (currentChar == '<') {
          PageElementTag tag = analysis.isInTag(currentIndex);
          if ((tag != null) &&
              (tag.getBeginIndex() == currentIndex) &&
              (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName()))) {
            currentIndex = tag.getEndIndex();
            endTag = Math.max(endTag, tag.getCompleteEndIndex());
            something = true;
            unclosedTags |= !tag.isComplete();
            if (tag.getCompleteBeginIndex() < beginLine) {
              emptyLine = false;
            }
          } else {
            emptyLine = false;
          }
        } else if (currentChar == '\'') {
          something = true;
          int count = 1;
          while ((currentIndex + count < contents.length()) &&
                 (contents.charAt(currentIndex + count) == '\'')) {
            count++;
          }
          switch (count) {
          case 2:
            countItalic++;
            break;
          case 3:
            countBold++;
            break;
          case 5:
            countItalic++;
            countBold++;
            break;
          default:
            emptyLine = false;
          }
          currentIndex += count;
        } else if (currentChar == ' ') {
          currentIndex++;
        } else if (currentChar == '\n') {
          break;
        } else {
          emptyLine = false;
        }
      }

      // Go to the end of the line
      while ((currentIndex < contents.length()) &&
          (contents.charAt(currentIndex) != '\n')) {
        currentIndex++;
      }
      if (currentIndex < contents.length()) {
        currentIndex++;
      }

      // Test if error should be reported
      boolean shouldReport = false;
      if (emptyLine && something && (endTag <= currentIndex)) {
        shouldReport = true;
      }
      if (shouldReport) {
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, beginLine) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, beginLine) != null)) {
          shouldReport = false;
        }
      }

      // Report error if needed
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Decide if it can be automatic
        boolean automatic = !unclosedTags;
        if ((countItalic % 2 != 0) || (countBold % 2 != 0)) {
          automatic = false;
        }

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginLine, currentIndex);
        errorResult.addReplacement("", automatic);
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
