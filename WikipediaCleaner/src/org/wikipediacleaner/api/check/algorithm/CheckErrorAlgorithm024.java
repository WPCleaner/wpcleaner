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
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 24 of check wikipedia project.
 * Error 24: Pre not correct end
 */
public class CheckErrorAlgorithm024 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm024() {
    super("Pre not correct end");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Check every <pre> tag
    List<PageElementTag> preTags = pageAnalysis.getTags(PageElementTag.TAG_WIKI_PRE);
    boolean result = false;
    for (PageElementTag preTag : preTags) {
      int beginIndex = preTag.getBeginIndex();
      if (!preTag.isFullTag() &&
          !preTag.isComplete() &&
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, beginIndex) == null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            beginIndex, preTag.getEndIndex());
        errorResult.addReplacement("", GT._("Delete"));
        errors.add(errorResult);
      }
    }
    return result;
  }
}
