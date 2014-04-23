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
 * Algorithm for analyzing error 100 of check wikipedia project.
 * Error 100: List tag (&lt;ol&gt;, &lt;ul&gt; or &lt;li&gt;) with no correct match.
 */
public class CheckErrorAlgorithm100 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm100() {
    super("List tag (<ol>, <ul> or <li>) with no correct match.");
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

    // Check every type of tag
    boolean result = false;
    result |= analyzeTag(analysis, PageElementTag.TAG_HTML_OL, errors);
    result |= analyzeTag(analysis, PageElementTag.TAG_HTML_UL, errors);
    result |= analyzeTag(analysis, PageElementTag.TAG_HTML_LI, errors);

    return result;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param tagName Tag to search for.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis, String tagName,
      Collection<CheckErrorResult> errors) {

    // Check every tag
    List<PageElementTag> tags = analysis.getCompleteTags(tagName);
    if ((tags == null) || (tags.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTag tag : tags) {
      if (!tag.isComplete()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tag.getBeginIndex(), tag.getEndIndex());
        errors.add(errorResult);
      }
    }
    return result;
  }
}
