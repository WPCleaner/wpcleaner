/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 529 of check wikipedia project.
 * Error 529: ISBN magical link
 */
public class CheckErrorAlgorithm529 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm529() {
    super("ISBN magical link");
  }

  /** Tracking category. */
  private String trackingCategory;

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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each PMID
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    if ((isbns == null) || (isbns.isEmpty())) {
      return false;
    }
    for (PageElementISBN isbn : isbns) {
      if (!isbn.isTemplateParameter() && isbn.isCorrect()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, isbn.getBeginIndex(), isbn.getEndIndex());
        List<String[]> isbnTemplates = analysis.getWPCConfiguration().getStringArrayList(
            WPCConfigurationStringList.ISBN_TEMPLATES);
        if (isbnTemplates != null) {
          for (String[] isbnTemplate : isbnTemplates) {
            if (isbnTemplate.length > 2) {
              String templateName = isbnTemplate[0];
              String[] params = isbnTemplate[1].split(",");
              Boolean suggested = Boolean.valueOf(isbnTemplate[2]);
              if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
                StringBuilder replacement = new StringBuilder();
                replacement.append("{{");
                replacement.append(templateName);
                replacement.append("|");
                if (!"1".equals(params[0])) {
                  replacement.append(params[0]);
                  replacement.append("=");
                }
                replacement.append(isbn.getISBNNotTrimmed());
                replacement.append("}}");
                errorResult.addReplacement(replacement.toString());
              }
            }
          }
        }
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (getTrackingCategory() != null);
  }

  /**
   * @param category Tracking category.
   */
  public void setTrackingCategory(String category) {
    trackingCategory = category;
  }

  /**
   * @return Tracking category.
   */
  private String getTrackingCategory() {
    String categoryName = getSpecificProperty("category", true, true, false);
    if ((categoryName != null) &&
        (categoryName.trim().length() > 0)) {
      return categoryName;
    }
    if ((trackingCategory != null) &&
        (trackingCategory.trim().length() > 0)) {
      return trackingCategory;
    }
    return null;
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    List<Page> result = null;
    String categoryName = getTrackingCategory();
    if (categoryName != null) {
      API api = APIFactory.getAPI();
      String title = wiki.getWikiConfiguration().getPageTitle(Namespace.CATEGORY, categoryName);
      Page category = DataManager.getPage(wiki, title, null, null, null);
      try {
        api.retrieveCategoryMembers(wiki, category, 0, false, limit);
        result = category.getRelatedPages(RelatedPages.CATEGORY_MEMBERS);
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("category", GT._("A category containing the list of pages in error"));
    return parameters;
  }
}
