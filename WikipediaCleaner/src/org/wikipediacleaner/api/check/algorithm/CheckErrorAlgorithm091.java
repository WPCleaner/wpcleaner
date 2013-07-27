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
public class CheckErrorAlgorithm091 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm091() {
    super("DEFAULTSORT is missing and title with lowercase letters");
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

    // Checking if title contains lower case letters
    boolean lowFound = false;
    boolean firstLetter = true;
    String title = pageAnalysis.getPage().getTitle();
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
    List<PageElementFunction> defaultSorts = pageAnalysis.getDefaultSorts();
    if (defaultSorts.size() > 0) {
      return false;
    }

    // Searching for Categories without a sort key
    boolean categoriesWithoutSort = false;
    List<PageElementCategory> categories = pageAnalysis.getCategories();
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
        pageAnalysis.getPage(), category.getBeginIndex(), category.getEndIndex());
    String replacement =
        createDefaultSort(pageAnalysis) + "\n" +
    pageAnalysis.getContents().substring(category.getBeginIndex(), category.getEndIndex());
    errorResult.addReplacement(replacement, GT._("Add DEFAULTSORT"));
    errors.add(errorResult);
    return true;
  }
}
