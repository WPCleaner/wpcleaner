/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;


/**
 * Algorithm for analyzing error 17 of check wikipedia project.
 * Error 17: Category duplication
 */
public class CheckErrorAlgorithm017 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm017() {
    super("Category duplication");
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

    boolean result = false;
    HashMap<String, PageElementCategory> categories = new HashMap<String, PageElementCategory>();
    ArrayList<String> categoriesTwice = new ArrayList<String>();
    for (PageElementCategory category : pageAnalysis.getCategories()) {
      PageElementCategory existingCategory = categories.get(category.getName());
      if (existingCategory == null) {
        categories.put(category.getName(), category);
      } else {
        if (errors == null) {
          return true;
        }
        result = true;
        if (!categoriesTwice.contains(category.getName())) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              existingCategory.getBeginIndex(),
              existingCategory.getEndIndex(),
              CheckErrorResult.ErrorLevel.CORRECT);
          errors.add(errorResult);
          categoriesTwice.add(category.getName());
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            category.getBeginIndex(),
            category.getEndIndex());
        errorResult.addReplacement("");
        errors.add(errorResult);
      }
    }

    return result;
  }
}
