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
import org.wikipediacleaner.api.data.PageElementTitle;


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
    int firstTitle = titles.get(0).getLevel();
    if (firstTitle < 3) {
      return false;
    }

    // Check every title
    boolean result = false;
    for (PageElementTitle title : titles) {
      if (title.getLevel() < firstTitle) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            title.getBeginIndex(), title.getEndIndex());
        errorResult.addEditTocAction(title);
        errors.add(errorResult);
      }
    }

    return result;
  }
}
