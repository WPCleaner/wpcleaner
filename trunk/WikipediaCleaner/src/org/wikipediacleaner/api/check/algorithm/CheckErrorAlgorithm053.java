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
import org.wikipediacleaner.api.data.PageElementTitle;


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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }

    // Retrieving last category and headline
    List<PageElementCategory> categories = analysis.getCategories();
    if (categories.size() == 0) {
      return false;
    }
    int lastCategory = categories.get(categories.size() - 1).getEndIndex();
    List<PageElementTitle> titles = analysis.getTitles();
    int lastTitle = (titles.size() > 0) ? (titles.get(titles.size() - 1).getEndIndex()) : 0;

    // Check every language link
    List<PageElementLanguageLink> languages = analysis.getLanguageLinks();
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementLanguageLink language : languages) {
      int begin = language.getBeginIndex();
      if (begin >= lastCategory) {
        return result;
      }
      if (begin > lastTitle) {
        if (errors == null) {
          return true;
        }
        result = true;
        int end = language.getEndIndex();
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis.getPage(), begin, end);
        errorResult.addReplacement("[[:" + contents.substring(begin + 2, end));
        errors.add(errorResult);
      }
    }

    return result;
  }
}
