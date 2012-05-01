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
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 88 of check wikipedia project.
 * Error 88: DEFAULTSORT with blank at first position
 */
public class CheckErrorAlgorithm088 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix DEFAULTSORT"),
  };

  public CheckErrorAlgorithm088() {
    super("DEFAULTSORT with blank at first position");
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

    // Check every DEFAULTSORT
    List<PageElementDefaultsort> defaultSorts = pageAnalysis.getDefaultSorts();
    boolean result = false;
    for (PageElementDefaultsort defaultSort : defaultSorts) {
      String text = defaultSort.getValueNotTrimmed();
      if ((text != null) && (text.startsWith(" "))) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            defaultSort.getBeginIndex(), defaultSort.getEndIndex());
        errorResult.addReplacement(PageElementDefaultsort.createDefaultsort(
            defaultSort.getTag(), text));
        errors.add(errorResult);
      }
    }

    return result;
  }
  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  @Override
  public String automaticFix(Page page, String contents) {
    return fix(globalFixes[0], page, contents);
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
    return fixUsingFirstReplacement(fixName, page, contents);
  }
}
