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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 91 of check wikipedia project.
 * Error 91: DEFAULTSORT is missing and title with lower case letters
 */
public class CheckErrorAlgorithm091 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Add DEFAULTSORT"),
  };

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
    List<PageElementDefaultsort> defaultSorts = pageAnalysis.getDefaultSorts();
    if (defaultSorts.size() > 0) {
      return false;
    }

    // Searching for Categories without a sort key
    List<PageElementCategory> categories = pageAnalysis.getCategories();
    for (PageElementCategory category : categories) {
      if ((category.getSort() == null) ||
          (category.getSort().trim().length() == 0)) {
        return true;
      }
    }

    return false;
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, Page page, String contents) {
    PageAnalysis pageAnalysis = new PageAnalysis(page, contents);
    return addDefaultSort(pageAnalysis);
  }
}
