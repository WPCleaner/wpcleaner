/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a02x.a022;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.string.CharacterUtils;


/**
 * Algorithm for analyzing error 22 of check wikipedia project.
 * Error 22: Category with space
 */
public class CheckErrorAlgorithm022 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Fix categories"),
  };

  public CheckErrorAlgorithm022() {
    super("Category with space");
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

    // Check category name space
    Namespace categoryNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    if (categoryNamespace == null) {
      return false;
    }
    String preferredCategory = categoryNamespace.getTitle();
    if (preferredCategory == null) {
      preferredCategory = PageElementCategory.DEFAULT_NAME;
    }

    // Check every category
    List<PageElementCategory> categories = analysis.getCategories();
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
      String sortSimple = sortFull;
      if (sortFull != null) {
        int firstSpace = 0;
        while ((firstSpace < sortFull.length()) &&
               (Character.isWhitespace(sortFull.charAt(firstSpace)))) {
          firstSpace++;
        }
        int lastSpace = sortFull.length();
        while ((lastSpace > firstSpace) &&
               (Character.isWhitespace(sortFull.charAt(lastSpace - 1)))) {
          lastSpace--;
        }
        if (lastSpace < sortFull.length()) {
          spaceFound = true;
          sortSimple = sortFull.substring(0, lastSpace);
        }
      }

      // Register error
      if (spaceFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        nameSimple = CharacterUtils.ucFirst(nameSimple);
        String namespace = CharacterUtils.trim(category.getCategoryNotTrimmed());
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            category.getBeginIndex(), category.getEndIndex());
        if (sortSimple == null) {
          errorResult.addReplacement(
              "[[" + namespace + ":" + nameSimple + "]]", true);
          if (!namespace.equals(preferredCategory)) {
            errorResult.addReplacement(
                "[[" + preferredCategory + ":" + nameSimple + "]]");
          }
        } else {
          errorResult.addReplacement(
              "[[" + namespace + ":" + nameSimple + "|" + sortSimple + "]]", true);
          if (!namespace.equals(preferredCategory)) {
            errorResult.addReplacement(
                "[[" + preferredCategory + ":" + nameSimple + "|" + sortSimple + "]]");
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
