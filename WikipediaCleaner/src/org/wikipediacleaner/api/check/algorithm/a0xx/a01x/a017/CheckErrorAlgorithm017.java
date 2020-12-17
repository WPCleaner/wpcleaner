/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a01x.a017;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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
    GT._T("Delete all"),
  };

  public CheckErrorAlgorithm017() {
    super("Category duplication");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Ignore redirect pages
    if (analysis.getPage().getRedirects().isRedirect()) {
      // TODO: Be more subtle, remove only the first category if it's the target of the redirection
      return false;
    }

    // Case sensitiveness
    Namespace namespace = analysis.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    EnumCaseSensitiveness sensitive = EnumCaseSensitiveness.UNKNOWN;
    if (namespace != null) {
      sensitive = namespace.getCaseSensitiveness();
    }

    // Group categories by name
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return false;
    }
    Map<String, List<PageElementCategory>> groupedCategories = new HashMap<String, List<PageElementCategory>>();
    for (PageElementCategory category : categories) {
      String name = sensitive.normalize(category.getName());
      List<PageElementCategory> groupCategory = groupedCategories.get(name);
      if (groupCategory == null) {
        groupCategory = new ArrayList<PageElementCategory>();
        groupedCategories.put(name, groupCategory);
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
      
      // Group categories
      String name = sensitive.normalize(category.getName());
      List<PageElementCategory> groupCategory = groupedCategories.get(name);

      // Report error if more than one category in the group
      if ((groupCategory != null) && (groupCategory.size() > 1)) {
        if (errors == null) {
          return true;
        }
        result = true;
        PageElementCategory keepCategory = keepCategory(groupCategory, lastTitle);
        if (keepCategory == category) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
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
              analysis, beginIndex, endIndex);
          errorResult.addReplacement("", GT._T("Delete"), automatic);
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
  protected String internalAutomaticFix(PageAnalysis analysis) {
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
