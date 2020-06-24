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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 49 of check wikipedia project.
 * Error 49: Headline with HTML
 */
public class CheckErrorAlgorithm049 extends CheckErrorAlgorithmBase {

  /**
   * Tags that can be detected.
   */
  private static final String[] titleTags = {
    PageElementTag.TAG_HTML_H1,
    PageElementTag.TAG_HTML_H2,
    PageElementTag.TAG_HTML_H3,
    PageElementTag.TAG_HTML_H4,
    PageElementTag.TAG_HTML_H5,
    PageElementTag.TAG_HTML_H6,
    PageElementTag.TAG_HTML_H7,
    PageElementTag.TAG_HTML_H8,
    PageElementTag.TAG_HTML_H9
  };

  public CheckErrorAlgorithm049() {
    super("Headline with HTML");
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

    // Analyzing each possible tag
    boolean result = false;
    for (int level = 0; level < titleTags.length; level++) {
      String tagName = titleTags[level];
      List<PageElementTag> tags = analysis.getCompleteTags(tagName);
      if (tags != null) {
        for (PageElementTag tag : tags) {
          // Decide if error should be reported
          boolean shouldReport = true;
          int index = tag.getBeginIndex();
          if (shouldReport &&
              ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
               (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null))) {
            shouldReport = false;
          }

          // Report error
          if (shouldReport) {
            if (errors == null) {
              return true;
            }
            result = true;
  
            // Find possible replacement
            String replacement = analysis.getContents().substring(
                tag.getValueBeginIndex(), tag.getValueEndIndex());
  
            // Create error
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
            errorResult.addReplacement(PageElementTitle.createTitle(
                level + 1, replacement, null));
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
