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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 22 of check wikipedia project.
 * Error 22: Category with space
 */
public class CheckErrorAlgorithm022 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix categories"),
  };

  public CheckErrorAlgorithm022() {
    super("Category with space");
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

    // Check category name space
    Namespace categoryNamespace = Namespace.getNamespace(
        Namespace.CATEGORY, pageAnalysis.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return false;
    }
    String preferredCategory = categoryNamespace.getTitle();
    if (preferredCategory == null) {
      preferredCategory = PageElementCategory.DEFAULT_NAME;
    }

    // Check every category
    List<PageElementCategory> categories = pageAnalysis.getCategories();
    boolean result = false;
    for (PageElementCategory category : categories) {

      // Check if space was found
      boolean spaceFound = false;
      String categoryFull = category.getCategoryNotTrimmed();
      String categorySimple = category.getCategory();
      if ((categoryFull != null) &&
          (categorySimple != null) &&
          (categoryFull.length() > categorySimple.length())) {
        spaceFound = true;
      }
      String nameFull = category.getNameNotTrimmed();
      String nameSimple = category.getName();
      if ((nameFull != null) &&
          (nameSimple != null) &&
          (nameFull.length() > nameSimple.length())) {
        spaceFound = true;
      }
      String sortFull = category.getSortNotTrimmed();

      // Register error
      if (spaceFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        nameSimple = Page.getStringUcFirst(nameSimple);
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            category.getBeginIndex(), category.getEndIndex());
        if (sortFull == null) {
          errorResult.addReplacement(
              "[[" + preferredCategory + ":" + nameSimple + "]]");
        } else {
          errorResult.addReplacement(
              "[[" + preferredCategory + ":" + nameSimple + "|" + sortFull + "]]");
        }
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
