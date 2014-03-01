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
 * Algorithm for analyzing error 98 of check wikipedia project.
 * Error 98: Subscript not correct end
 */
public class CheckErrorAlgorithm098 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm098() {
    super("Subscript not correct end");
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
    List<PageElementTag> subTags = pageAnalysis.getTags(PageElementTag.TAG_HTML_SUB);
    boolean result = false;
    for (PageElementTag subTag : subTags) {
      int beginIndex = subTag.getBeginIndex();
      if (!subTag.isEndTag() &&
          !subTag.isComplete() &&
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, beginIndex) == null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            beginIndex, subTag.getEndIndex());
        errorResult.addReplacement("", GT._("Delete"));
        errors.add(errorResult);
      }
    }
    return result;
  }
}
