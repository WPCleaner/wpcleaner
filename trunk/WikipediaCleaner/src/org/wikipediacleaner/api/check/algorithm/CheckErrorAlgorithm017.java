/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }

    // Group categories by name
    List<PageElementCategory> categories = analysis.getCategories();
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
    List<PageElementTitle> titles = analysis.getTitles();
    int lastTitle = 0;
    if ((titles != null) && (!titles.isEmpty())) {
      lastTitle = titles.get(titles.size() - 1).getEndIndex();
    }

    // Check each category
    boolean result = false;
    String contents = analysis.getContents();
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
              analysis.getPage(),
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

          // Decide in the fix can be automatic
          boolean automatic = false;
          if (category.getBeginIndex() > keepCategory.getBeginIndex()) {
            int currentIndex = keepCategory.getEndIndex();
            boolean finished = false;
            while (!finished && (currentIndex < category.getBeginIndex())) {
              char currentChar = contents.charAt(currentIndex);
              if ((currentChar == ' ') || (currentChar == '\n')) {
                currentIndex++;
              } else {
                PageElementCategory nextCategory = analysis.isInCategory(currentIndex);
                if (nextCategory != null) {
                  currentIndex = nextCategory.getEndIndex();
                } else {
                  finished = true;
                }
              }
            }
            if (currentIndex >= keepCategory.getBeginIndex()) {
              if ((category.getSort() != null) &&
                  (category.getSort().length() > 0)) {
                if (category.getSort().equals(keepCategory.getSort())) {
                  automatic = true;
                }
              } else {
                if ((keepCategory.getSort() == null) ||
                    (keepCategory.getSort().length() == 0)) {
                  automatic = true;
                }
              }
            }
          }

          // Mark the error
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(),
              beginIndex, endIndex);
          errorResult.addReplacement("", GT._("Delete"), automatic);
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
    return fixUsingAutomaticReplacement(analysis);
  }
}
