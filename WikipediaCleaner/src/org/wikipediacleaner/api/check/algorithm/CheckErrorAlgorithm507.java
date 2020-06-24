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
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 507 of check wikipedia project.
 * Error 507: Gallery without caption
 */
public class CheckErrorAlgorithm507 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm507() {
    super("Gallery without caption");
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

    // Analyze each gallery tag
    List<PageElementTag> galleryTags = analysis.getTags(PageElementTag.TAG_WIKI_GALLERY);
    boolean result = false;
    for (PageElementTag galleryTag : galleryTags) {
      if (!galleryTag.isFullTag() && !galleryTag.isEndTag()) {
        Parameter description = galleryTag.getParameter("caption");
        if ((description == null) ||
            (description.getTrimmedValue() == null) ||
            (description.getTrimmedValue().length() == 0)) {
          if (errors == null) {
            return true;
          }
          result = true;

          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, galleryTag.getBeginIndex(), galleryTag.getEndIndex());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
