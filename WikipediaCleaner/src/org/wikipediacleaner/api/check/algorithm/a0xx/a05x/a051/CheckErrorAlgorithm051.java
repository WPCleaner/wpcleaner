/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a05x.a051;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 51 of check wikipedia project.
 * Error 51: Interwiki before last headline.
 */
public class CheckErrorAlgorithm051 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm051() {
    super("Interwiki before last headline");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Retrieving last headline
    List<PageElementTitle> titles = analysis.getTitles();
    if (titles.size() == 0) {
      return false;
    }
    int lastTitle = titles.get(titles.size() - 1).getEndIndex();

    // Checking every language link
    List<PageElementLanguageLink> languages = analysis.getLanguageLinks();
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementLanguageLink language : languages) {
      if (language.getBeginIndex() >= lastTitle) {
        return result;
      }
      if (errors == null) {
        return true;
      }
      result = true;
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis,
          language.getBeginIndex(), language.getEndIndex());
      errorResult.addReplacement(
          "[[:" + contents.substring(language.getBeginIndex() + 2, language.getEndIndex()));
      errors.add(errorResult);
    }

    return result;
  }
}
