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

import java.util.Collection;
import java.util.HashMap;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 92 of check wikipedia project.
 * Error 92: Headline double
 */
public class CheckErrorAlgorithm092 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm092() {
    super("Headline double");
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
    int previousTitleLevel = 0;
    HashMap<Integer, HashMap<String, PageElementTitle>> titles =
        new HashMap<Integer, HashMap<String, PageElementTitle>>();
    for (PageElementTitle title : pageAnalysis.getTitles()) {

      // Clean up titles with a lower level
      int titleLevel = title.getFirstLevel();
      if (titleLevel < previousTitleLevel) {
        for (int i = previousTitleLevel; i > titleLevel; i--) {
          titles.remove(Integer.valueOf(i));
        }
      }

      // Analyze current level
      HashMap<String, PageElementTitle> knownTitles = titles.get(Integer.valueOf(titleLevel));
      String titleValue = title.getTitle();
      if (knownTitles == null) {
        knownTitles = new HashMap<String, PageElementTitle>();
        knownTitles.put(titleValue, title);
        titles.put(Integer.valueOf(titleLevel), knownTitles);
      } else if (!knownTitles.containsKey(titleValue)) {
        knownTitles.put(titleValue, title);
      } else {
        if (errors == null) {
          return true;
        }
        result = true;
        PageElementTitle previousTitle = knownTitles.get(titleValue);
        if (previousTitle != null) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              previousTitle.getBeginIndex(), previousTitle.getEndIndex(),
              ErrorLevel.CORRECT);
          errors.add(errorResult);
          knownTitles.put(titleValue, null);
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            title.getBeginIndex(), title.getEndIndex());
        errors.add(errorResult);
      }

      previousTitleLevel = titleLevel;
    }

    return result;
  }
}
