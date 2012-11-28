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
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 17 of check wikipedia project.
 * Error 17: Category duplication
 */
public class CheckErrorAlgorithm017 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Delete all"),
  };

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

    // Group categories by name
    List<PageElementCategory> categories = pageAnalysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return false;
    }
    Map<String, List<PageElementCategory>> groupedCategories = new HashMap<String, List<PageElementCategory>>();
    for (PageElementCategory category : categories) {
      List<PageElementCategory> groupCategory = groupedCategories.get(category.getName());
      if (groupCategory == null) {
        groupCategory = new ArrayList<PageElementCategory>();
        groupedCategories.put(category.getName(), groupCategory);
      }
      groupCategory.add(category);
    }

    // Compute index of last title
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    int lastTitle = 0;
    if ((titles != null) && (!titles.isEmpty())) {
      lastTitle = titles.get(titles.size() - 1).getEndIndex();
    }

    // Check each category
    boolean result = false;
    String contents = pageAnalysis.getContents();
    for (PageElementCategory category : categories) {
      List<PageElementCategory> groupCategory = groupedCategories.get(category.getName());
      if ((groupCategory != null) && (groupCategory.size() > 1)) {
        if (errors == null) {
          return true;
        }
        result = true;
        PageElementCategory keepCategory = keepCategory(groupCategory, lastTitle);
        if (keepCategory == category) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              category.getBeginIndex(),
              category.getEndIndex(),
              CheckErrorResult.ErrorLevel.CORRECT);
          errors.add(errorResult);
        } else {
          int beginIndex = category.getBeginIndex();
          while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == ' ')) {
            beginIndex--;
          }
          boolean beginLine = (beginIndex == 0) || (contents.charAt(beginIndex - 1) == '\n');
          int endIndex = category.getEndIndex();
          while ((endIndex < contents.length()) && (contents.charAt(endIndex) == ' ')) {
            endIndex++;
          }
          boolean endLine = (endIndex >= contents.length()) || (contents.charAt(endIndex) == '\n');
          if (beginLine && endLine) {
            endIndex = Math.min(endIndex + 1, contents.length());
          }
          if (!beginLine) {
            beginIndex = category.getBeginIndex();
          }
          if (!endLine) {
            endIndex = category.getEndIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              beginIndex, endIndex);
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param categories List of categories for the same name.
   * @param lastTitle Index of the last title.
   * @return Which category should be kept.
   */
  private PageElementCategory keepCategory(
      List<PageElementCategory> categories, int lastTitle) {

    // First: category after last title and with sort key
    for (PageElementCategory category : categories) {
      if ((category.getBeginIndex() >= lastTitle) &&
          (category.getSort() != null) &&
          (category.getSort().length() > 0)) {
        return category;
      }
    }

    // Second: category after last title
    for (PageElementCategory category : categories) {
      if (category.getBeginIndex() >= lastTitle) {
        return category;
      }
    }

    // Third: category with sort key
    for (PageElementCategory category : categories) {
      if ((category.getSort() != null) &&
          (category.getSort().length() > 0)) {
        return category;
      }
    }

    // Last: last category
    return categories.get(categories.size() - 1);
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public String botFix(PageAnalysis analysis) {
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
