/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
    GT._T("Fix categories"),
  };

  public CheckErrorAlgorithm018() {
    super("Category first letter small");
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
    Namespace categoryNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    if (categoryNamespace == null) {
      return false;
    }
    Collection<PageElementCategory> categories = analysis.getCategories();
    if (categories == null) {
      return false;
    }
    boolean result = false;
    for (PageElementCategory category : categories) {
      String namespace = category.getCategoryNotTrimmed().trim();
      boolean lowerCaseNamespace = false;
      if (namespace == null) {
        namespace = categoryNamespace.getTitle();
      } else if ((namespace.length() > 0) &&
                 (Character.isLowerCase(namespace.charAt(0)))) {
        lowerCaseNamespace = true;
        namespace = "" + Character.toUpperCase(namespace.charAt(0)) + namespace.substring(1);
      }
      String categoryName = category.getNameNotTrimmed();
      if (categoryName != null) {
        categoryName = categoryName.trim();
      }
      boolean lowerCaseName = false;
      if ((categoryName != null) &&
          (categoryName.length() > 0) &&
          (Character.isLowerCase(categoryName.charAt(0)))) {
        lowerCaseName = true;
        categoryName = "" + Character.toUpperCase(categoryName.charAt(0)) + categoryName.substring(1);
      }
      String categorySort = category.getSort();
      if ("".equals(categorySort)) {
        categorySort = category.getSortNotTrimmed();
      }
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
            analysis, category.getBeginIndex(), category.getEndIndex());
        if (categorySort != null) {
          errorResult.addReplacement(
              "[[" + namespace + ":" + categoryName + "|" + categorySort + "]]",
              true);
          if (!namespace.equals(categoryNamespace.getTitle())) {
            errorResult.addReplacement(
                "[[" + categoryNamespace.getTitle() + ":" + categoryName + "|" + categorySort + "]]",
                false);
          }
        } else {
          errorResult.addReplacement(
              "[[" + namespace + ":" + categoryName + "]]",
              true);
          if (!namespace.equals(categoryNamespace.getTitle())) {
            errorResult.addReplacement(
                "[[" + categoryNamespace.getTitle() + ":" + categoryName + "]]",
                false);
          }
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
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
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
