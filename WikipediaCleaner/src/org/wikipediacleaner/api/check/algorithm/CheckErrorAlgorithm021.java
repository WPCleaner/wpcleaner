/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckCategoryLinkActionProvider;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 21 of check wikipedia project.
 * Error 21: Category is English
 */
public class CheckErrorAlgorithm021 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm021() {
    super("Category is English");
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
    if (EnumWikipedia.EN.equals(analysis.getWikipedia())) {
      return false;
    }
    Namespace categoryNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    if (categoryNamespace == null) {
      return false;
    }
    String preferredCategory = categoryNamespace.getTitle();
    if (PageElementCategory.DEFAULT_NAME.equalsIgnoreCase(preferredCategory)) {
      return false;
    }

    // Check every category
    List<PageElementCategory> categories = analysis.getCategories();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementCategory category : categories) {
      if (PageElementCategory.DEFAULT_NAME.equalsIgnoreCase(category.getCategory())) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, category.getBeginIndex(), category.getEndIndex());
        errorResult.addPossibleAction(
            GT._T("Check category"),
            new CheckCategoryLinkActionProvider(
                EnumWikipedia.EN, analysis.getWikipedia(),
                category.getName(), category.getSort()));
        List<String> replacements = new ArrayList<String>();
        if ((preferredCategory != null) &&
            (categoryNamespace.isPossibleName(preferredCategory))) {
          replacements.add(preferredCategory);
        }
        if (!replacements.contains(categoryNamespace.getCanonicalTitle())) {
          replacements.add(categoryNamespace.getCanonicalTitle());
        }
        for (String alias : categoryNamespace.getAliases()) {
          if (!replacements.contains(alias)) {
            replacements.add(alias);
          }
        }
        for (String replacement : replacements) {
          if (!PageElementCategory.DEFAULT_NAME.equalsIgnoreCase(replacement)) {
            errorResult.addReplacement(
                "[[" + replacement + ":" + category.getName() +
                ((category.getSort() != null) ? "|" + category.getSort() : "") + "]]");
          }
        }
        errorResult.addReplacement(
            "<!-- " + contents.substring(category.getBeginIndex(), category.getEndIndex()) + " -->",
            GT._T("Comment category out"));
        errors.add(errorResult);
      }
    }

    return result;
  }
}
