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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 99 of check wikipedia project.
 * Error 99: Superscript not correct end
 */
public class CheckErrorAlgorithm099 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm099() {
    super("Superscript not correct end");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (pageAnalysis == null) {
      return false;
    }

    // Check every <math> tag
    List<PageElementTag> supTags = pageAnalysis.getTags(PageElementTag.TAG_HTML_SUP);
    boolean result = false;
    for (PageElementTag supTag : supTags) {
      int beginIndex = supTag.getBeginIndex();
      if (!supTag.isEndTag() &&
          !supTag.isComplete() &&
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, beginIndex) == null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            beginIndex, supTag.getEndIndex());
        errorResult.addReplacement("");
        errors.add(errorResult);
      } else if (supTag.isEndTag() && supTag.endWithSpace()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            beginIndex, supTag.getEndIndex(),
            ErrorLevel.WARNING);
        errorResult.addReplacement(
            PageElementTag.createTag(PageElementTag.TAG_HTML_SUP, true, false),
            true);
        errors.add(errorResult);
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
