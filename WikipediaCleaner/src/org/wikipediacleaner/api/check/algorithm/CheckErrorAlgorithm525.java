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
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 525 of check wikipedia project.
 * Error 525: Useless span tag
 */
public class CheckErrorAlgorithm525 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm525() {
    super("Useless span tag");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each tag
    List<PageElementTag> tags = analysis.getCompleteTags(PageElementTag.TAG_HTML_SPAN);
    if ((tags == null) || tags.isEmpty()) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTag tag : tags) {
      // Decide if tag is useful
      boolean isUseless = true;
      for (int numParam = 0; numParam < tag.getParametersCount(); numParam++) {
        Parameter param = tag.getParameter(numParam);
        String value = param.getTrimmedValue();
        if ((value != null) && (!value.isEmpty())) {
          String lang = analysis.getWikipedia().getSettings().getLanguage();
          if ("lang".equals(param.getName()) && (lang != null) && lang.equalsIgnoreCase(value)) {
            // useful
          } else {
            isUseless = false;
          }
        }
      }
      if (!tag.isComplete()) {
        isUseless = true;
      }

      if (isUseless) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
        if (tag.isFullTag() || !tag.isComplete()) {
          errorResult.addReplacement("");
        } else {
          String replacement = contents.substring(
              tag.getValueBeginIndex(), tag.getValueEndIndex());
          errorResult.addReplacement(
              replacement,
              GT._("Remove {0} tags", PageElementTag.TAG_HTML_SPAN));
        }
        errors.add(errorResult);
      }
    }

    return result;
  }
}
