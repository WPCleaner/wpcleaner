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
 * Algorithm for analyzing error 63 of check wikipedia project.
 * Error 63: HTML text style element &lt;small&gt; in ref, sub or sup
 */
public class CheckErrorAlgorithm063 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm063() {
    super("HTML text style element <small> in ref, sub or sup");
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

    // Analyze each <small> tag
    boolean result = false;
    List<PageElementTag> smallTags = pageAnalysis.getTags(PageElementTag.TAG_HTML_SMALL);
    for (PageElementTag smallTag : smallTags) {
      int index = smallTag.getBeginIndex();
      PageElementTag refTag = pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, index);
      PageElementTag subTag = pageAnalysis.getSurroundingTag(PageElementTag.TAG_HTML_SUB, index);
      PageElementTag supTag = pageAnalysis.getSurroundingTag(PageElementTag.TAG_HTML_SUP, index);
      if ((refTag != null) || (subTag != null) || (supTag != null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            smallTag.getBeginIndex(), smallTag.getEndIndex());
        errors.add(errorResult);
      }
    }

    return result;
  }
}
