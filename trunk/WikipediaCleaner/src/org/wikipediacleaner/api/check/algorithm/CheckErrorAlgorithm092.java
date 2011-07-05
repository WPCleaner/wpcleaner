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
    HashMap<Integer, ArrayList<String>> titles = new HashMap<Integer, ArrayList<String>>();
    for (PageElementTitle title : pageAnalysis.getTitles()) {

      // Clean up titles with a lower level
      int titleLevel = title.getFirstLevel();
      if (titleLevel < previousTitleLevel) {
        for (int i = previousTitleLevel; i > titleLevel; i--) {
          titles.remove(Integer.valueOf(i));
        }
      }

      // Analyze current level
      ArrayList<String> knownTitles = titles.get(Integer.valueOf(titleLevel));
      String titleValue = title.getTitle();
      if (knownTitles == null) {
        knownTitles = new ArrayList<String>();
        knownTitles.add(titleValue);
        titles.put(Integer.valueOf(titleLevel), knownTitles);
      } else if (!knownTitles.contains(titleValue)) {
        knownTitles.add(titleValue);
      } else {
        if (errors == null) {
          return true;
        }
        result = true;
        errors.add(createCheckErrorResult(
            pageAnalysis.getPage(),
            title.getBeginIndex(),
            title.getEndIndex()));
      }

      previousTitleLevel = titleLevel;
    }

    return result;
  }
}
