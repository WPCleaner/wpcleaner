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
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 83 of check wikipedia project.
 * Error 83: Headlines start with three "=" and later with level two
 */
public class CheckErrorAlgorithm083 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm083() {
    super("Headlines start with three \"=\" and later with level two");
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

    // Check first title
    List<PageElementTitle> titles = analysis.getTitles();
    if (titles.size() == 0) {
      return false;
    }
    PageElementTitle firstTitle = titles.get(0);
    if (firstTitle.getLevel() < 3) {
      return false;
    }

    // Check every title
    boolean result = false;
    for (int titleIndex = 1; titleIndex < titles.size(); titleIndex++) {
      PageElementTitle title = titles.get(titleIndex);
      if (title.getLevel() < firstTitle.getLevel()) {
        if (errors == null) {
          return true;
        }
        result = true;
        if (titleIndex == 1) {
          CheckErrorResult errorResult = createCheckErrorResult(analysis, firstTitle.getBeginIndex(), firstTitle.getEndIndex());
          errorResult.addReplacement(
              PageElementTitle.createTitle(title.getLevel(), firstTitle.getTitle(), firstTitle.getAfterTitle()),
              true);
          errors.add(errorResult);
          break;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            title.getBeginIndex(), title.getEndIndex());
        errorResult.addEditTocAction(title);
        errors.add(errorResult);
        break;
      }
    }

    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
