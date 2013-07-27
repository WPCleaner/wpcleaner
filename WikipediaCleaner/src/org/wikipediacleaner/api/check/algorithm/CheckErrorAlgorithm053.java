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
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementLanguageLink;


/**
 * Algorithm for analyzing error 53 of check wikipedia project.
 * Error 53: Interwiki before last category
 */
public class CheckErrorAlgorithm053 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm053() {
    super("Interwiki before last category");
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

    // Retrieving last category
    List<PageElementCategory> categories = pageAnalysis.getCategories();
    if (categories.size() == 0) {
      return false;
    }
    int lastCategory = categories.get(categories.size() - 1).getEndIndex();

    // Check every language link
    List<PageElementLanguageLink> languages = pageAnalysis.getLanguageLinks();
    boolean result = false;
    for (PageElementLanguageLink language : languages) {
      if (language.getBeginIndex() >= lastCategory) {
        return result;
      }
      if (errors == null) {
        return true;
      }
      result = true;
      CheckErrorResult errorResult = createCheckErrorResult(
          pageAnalysis.getPage(),
          language.getBeginIndex(), language.getEndIndex());
      errors.add(errorResult);
    }

    return result;
  }
}
