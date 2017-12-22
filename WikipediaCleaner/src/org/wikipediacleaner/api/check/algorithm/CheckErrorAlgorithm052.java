/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 52 of check wikipedia project.
 * Error 52: Category before last headline.
 */
public class CheckErrorAlgorithm052 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm052() {
    super("Category before last headline");
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

    // Searching for last headline
    Collection<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || (titles.isEmpty())) {
      return false;
    }
    PageElementTitle title = null;
    for (PageElementTitle tmpTitle : titles) {
      if ((title == null) || (title.getBeginIndex() < tmpTitle.getBeginIndex())) {
        title = tmpTitle;
      }
    }
    if (title == null) {
      return false;
    }

    // Checking every category
    boolean result = false;
    for (PageElementCategory category : analysis.getCategories()) {
      if (category.getBeginIndex() < title.getBeginIndex()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            category.getBeginIndex(),
            category.getEndIndex());
        String categoryName = category.getName();
        if ((categoryName == null) || ("".equals(categoryName))) {
          errorResult.addReplacement("", true);
        }
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
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
