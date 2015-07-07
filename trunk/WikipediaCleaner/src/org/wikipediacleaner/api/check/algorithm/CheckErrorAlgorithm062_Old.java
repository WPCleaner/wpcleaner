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
 * Algorithm for analyzing error 62 of check wikipedia project.
 * Error 62: Headline alone
 */
public class CheckErrorAlgorithm062_Old extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm062_Old() {
    super("Headline alone");
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

    // Retrieving titles
    boolean result = false;
    List<PageElementTitle> titles = analysis.getTitles();
    if (titles == null) {
      return false;
    }
    if (titles.size() < 5) {
      return false;
    }

    // Analyzing titles
    int previousLevel = 0;
    for (int i = 0; i < titles.size(); i++) {
      PageElementTitle title = titles.get(i);
      if ((title.getLevel() > previousLevel) &&
          (title.getLevel() > 2)) {
        int j = i + 1;
        int count = 0;
        while ((j < titles.size()) &&
               (titles.get(j).getLevel() >= title.getLevel())) {
          if (titles.get(j).getLevel() == title.getLevel()) {
            count++;
          }
          j++;
        }
        if (count == 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, title.getBeginIndex(), title.getEndIndex());
          errorResult.addEditTocAction();
          errors.add(errorResult);
        }
      }
      previousLevel = title.getLevel();
    }
    return result;
  }
}
