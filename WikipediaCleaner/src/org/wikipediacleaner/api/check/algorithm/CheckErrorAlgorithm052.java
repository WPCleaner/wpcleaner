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

    // Searching for last headline
    Collection<PageElementTitle> titles = pageAnalysis.getTitles();
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
    for (PageElementCategory category : pageAnalysis.getCategories()) {
      if (category.getBeginIndex() < title.getBeginIndex()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            category.getBeginIndex(),
            category.getEndIndex());
        errors.add(errorResult);
      }
    }

    return result;
  }
}
