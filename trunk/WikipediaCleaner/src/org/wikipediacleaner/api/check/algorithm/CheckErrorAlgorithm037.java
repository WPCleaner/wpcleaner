/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 37 of check wikipedia project.
 * Error 37: Title with special letters and no DEFAULTSORT
 */
public class CheckErrorAlgorithm037 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm037() {
    super("Title with special letters and no DEFAULTSORT");
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

    // Retrieve configuration
    int limit = 3;
    String firstCharaters = getSpecificProperty("first_characters", true, false, false);
    if (firstCharaters != null) {
      try {
        limit = Integer.valueOf(firstCharaters);
      } catch (NumberFormatException e) {
        //
      }
    }

    // Analyzing title to find special characters
    String title = pageAnalysis.getPage().getTitle();
    boolean characterFound = false;
    int currentPos = 0;
    while ((currentPos < title.length()) && (currentPos < limit)) {
      char character = title.charAt(currentPos);
      if (!SpecialCharacters.isAuthorized(character, pageAnalysis.getWikipedia())) {
        characterFound = true;
      }
      currentPos++;
    }
    if (!characterFound) {
      return false;
    }

    // Searching a DEFAULTSORT tag
    List<PageElementFunction> defaultSorts = pageAnalysis.getDefaultSorts();
    if ((defaultSorts != null) && (defaultSorts.size() > 0)) {
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
