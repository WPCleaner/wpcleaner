/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 536 of check wikipedia project.
 * Error 536: Tidy whitespace bug (see [[Special:LintErrors/tidy-whitespace-bug]])
 */
public class CheckErrorAlgorithm536 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm536() {
    super("Tidy whitespace bug");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each span tag
    List<PageElementTag> spanTags = analysis.getCompleteTags(PageElementTag.TAG_HTML_SPAN);
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTag spanTag : spanTags) {

      // Analyze span tag
      if (spanTag.isComplete() &&
          !spanTag.isEndTag() &&
          !spanTag.isFullTag() &&
          (spanTag.getMatchingTag() != null)) {

        boolean shouldReport = true;

        // Check value of span tag
        int valueBeginIndex = spanTag.getValueBeginIndex();
        int valueEndIndex = spanTag.getValueEndIndex();
        while ((valueBeginIndex < valueEndIndex) &&
               (" ".indexOf(contents.charAt(valueEndIndex - 1)) >= 0)) {
          valueEndIndex--;
        }
        if ((valueBeginIndex >= valueEndIndex) ||
            (valueEndIndex >= spanTag.getValueEndIndex())) {
           shouldReport = false;
        }

        // Check following tag
        int endIndex = spanTag.getCompleteEndIndex();
        if (shouldReport) {
          if ((endIndex >= contents.length()) ||
              (contents.charAt(endIndex) != '<')) {
            shouldReport = false;
          } else {
            PageElementTag nextTag = analysis.isInTag(endIndex);
            if (nextTag == null) {
              shouldReport = false;
            } else {
              String tagName = nextTag.getNormalizedName();
              if (!tagName.equals(PageElementTag.TAG_HTML_A) &&
                  !tagName.equals(PageElementTag.TAG_HTML_SPAN)) {
                shouldReport = false;
              }
            }
          }
        }

        // Report if needed
        if (shouldReport) {
          if (errors == null) {
            return true;
          }
          result = true;
          int beginIndex = spanTag.getBeginIndex();
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
          String replacement =
            contents.substring(beginIndex, valueEndIndex) +
            contents.substring(spanTag.getValueEndIndex(), endIndex) +
            contents.substring(valueEndIndex, spanTag.getValueEndIndex());
          String text =
            contents.substring(beginIndex, spanTag.getEndIndex()) + "..." +
            contents.substring(spanTag.getValueEndIndex(), endIndex) +
            contents.substring(valueEndIndex, spanTag.getValueEndIndex());
          errorResult.addReplacement(replacement, text, false);
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}