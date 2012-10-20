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

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 18 of check wikipedia project.
 * Error 18: Category first letter small
 */
public class CheckErrorAlgorithm018 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix categories"),
  };

  public CheckErrorAlgorithm018() {
    super("Category first letter small");
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
    Namespace categoryNamespace = Namespace.getNamespace(
        Namespace.CATEGORY, pageAnalysis.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return false;
    }
    Collection<PageElementCategory> categories = pageAnalysis.getCategories();
    if (categories == null) {
      return false;
    }
    boolean result = false;
    for (PageElementCategory category : categories) {
      String namespace = category.getCategory();
      boolean lowerCaseNamespace = false;
      if ((namespace != null) &&
          (namespace.length() > 0) &&
          (Character.isLowerCase(namespace.charAt(0)))) {
        lowerCaseNamespace = true;
        namespace = "" + Character.toUpperCase(namespace.charAt(0)) + namespace.substring(1);
      }
      String categoryName = category.getName();
      boolean lowerCaseName = false;
      if ((categoryName != null) &&
          (categoryName.length() > 0) &&
          (Character.isLowerCase(categoryName.charAt(0)))) {
        lowerCaseName = true;
        categoryName = "" + Character.toUpperCase(categoryName.charAt(0)) + categoryName.substring(1);
      }
      String categorySort = category.getSort();
      /*boolean lowerCaseSort = false;
      if ((categorySort != null) &&
          (categorySort.length() > 0) &&
          (Character.isLowerCase(categorySort.charAt(0)))) {
        lowerCaseSort = true;
        categorySort = "" + Character.toUpperCase(categorySort.charAt(0)) + categorySort.substring(1);
      }*/
      if (lowerCaseNamespace || lowerCaseName) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), category.getBeginIndex(), category.getEndIndex());
        if (categorySort != null) {
          errorResult.addReplacement(
              "[[" + categoryNamespace.getTitle() + ":" + categoryName + "|" + categorySort + "]]");
        } else {
          errorResult.addReplacement(
              "[[" + categoryNamespace.getTitle() + ":" + categoryName + "]]");
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public String automaticFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
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
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
