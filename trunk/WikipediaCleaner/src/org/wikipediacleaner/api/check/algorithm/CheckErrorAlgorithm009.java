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
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;


/**
 * Algorithm for analyzing error 09 of check wikipedia project.
 * Error 09: Categories more at one line
 */
public class CheckErrorAlgorithm009 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm009() {
    super("Categories more at one line");
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

    // Check every category
    List<PageElementCategory> categories = pageAnalysis.getCategories();
    if (categories.size() < 2) {
      return false;
    }
    int maxCategory = categories.size();
    boolean result = false;
    int currentCategory = 0;
    String contents = pageAnalysis.getContents();
    while (currentCategory < maxCategory) {
      // Group categories in the same line
      boolean endFound = false;
      int lastCategory = currentCategory;
      while ((!endFound) && (lastCategory < maxCategory - 1)) {
        int maxIndex = categories.get(lastCategory + 1).getBeginIndex();
        int currentIndex = categories.get(lastCategory).getEndIndex();
        while ((!endFound) && (currentIndex < maxIndex)) {
          if (contents.charAt(currentIndex) == '\n') {
            endFound = true;
          }
          currentIndex++;
        }
        if (!endFound) {
          lastCategory++;
        }
      }

      // Register error
      if (lastCategory > currentCategory) {
        if (errors == null) {
          return true;
        }
        result = true;

        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            categories.get(currentCategory).getBeginIndex(),
            categories.get(lastCategory).getEndIndex());
        StringBuilder replacement = new StringBuilder();
        for (int i = currentCategory; i < lastCategory; i++) {
          replacement.append(contents.substring(
              categories.get(i).getBeginIndex(),
              categories.get(i + 1).getBeginIndex()).trim());
          replacement.append('\n');
        }
        String replacementText = (lastCategory - currentCategory > 1) ?
            "[[...]]\u21B5...\u21B5[[...]]" : "[[...]]\u21B5[[...]]";
        replacement.append(contents.substring(
            categories.get(lastCategory).getBeginIndex(),
            categories.get(lastCategory).getEndIndex()));
        errorResult.addReplacement(replacement.toString(), replacementText);
        errors.add(errorResult);
      }
      currentCategory = lastCategory + 1;
    }

    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public String botFix(PageAnalysis analysis) {
    return fixUsingFirstReplacement("Replace", analysis);
  }
}
