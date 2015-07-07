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
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 91 of check wikipedia project.
 * Error 91: DEFAULTSORT is missing and title with lower case letters
 */
public class CheckErrorAlgorithm091_Old extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm091_Old() {
    super("DEFAULTSORT is missing and title with lowercase letters");
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

    // Checking if title contains lower case letters
    boolean lowFound = false;
    boolean firstLetter = true;
    String title = analysis.getPage().getTitle();
    for (int currentPos = 0; currentPos < title.length(); currentPos++) {
      if (Character.isLowerCase(title.charAt(currentPos))) {
        if (firstLetter) {
          lowFound = true;
        }
        firstLetter = false;
      } else if (Character.isWhitespace(title.charAt(currentPos))) {
        firstLetter = true;
      } else {
        firstLetter = false;
      }
    }
    if (!lowFound) {
      return false;
    }

    // Searching a DEFAULTSORT tag
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if (defaultSorts.size() > 0) {
      return false;
    }

    // Searching for Categories without a sort key
    boolean categoriesWithoutSort = false;
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return false;
    }
    for (PageElementCategory category : categories) {
      if ((category.getSort() == null) ||
          (category.getSort().trim().length() == 0)) {
        categoriesWithoutSort = true;
      }
    }
    if (!categoriesWithoutSort) {
      return false;
    }

    // Reporting error
    if (errors == null) {
      return true;
    }
    PageElementCategory category = categories.get(0);
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, category.getBeginIndex(), category.getEndIndex());
    String replacement =
        createDefaultSort(analysis) + "\n" +
    analysis.getContents().substring(category.getBeginIndex(), category.getEndIndex());
    errorResult.addReplacement(replacement, GT._("Add DEFAULTSORT"));
    errors.add(errorResult);
    return true;
  }
}
