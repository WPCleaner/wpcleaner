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

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 527 of check wikipedia project.
 * Error 527: Reference with same name but different content
 */
public class CheckErrorAlgorithm527 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm527() {
    super("Reference with same name but different content");
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

    // Group all references by name
    List<PageElementTag> refTags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if ((refTags == null) || (refTags.isEmpty())) {
      return false;
    }
    HashMap<String, List<PageElementTag>> namedRefTags = new HashMap<>();
    String content = analysis.getContents();
    for (PageElementTag refTag : refTags) {
      if (refTag.isComplete() && !refTag.isFullTag()) {
        Parameter paramName = refTag.getParameter("name");
        String value = content.substring(refTag.getValueBeginIndex(), refTag.getValueEndIndex());
        if ((paramName != null) &&
            (paramName.getValue() != null) &&
            (value != null) &&
            (value.length() > 0)) {
          List<PageElementTag> namedTags = namedRefTags.get(paramName.getValue());
          if (namedTags == null) {
            namedTags = new ArrayList<>();
            namedRefTags.put(paramName.getValue(), namedTags);
          }
          namedTags.add(refTag);
        }
      }
    }

    // Analyze for errors
    boolean result = false;
    for (List<PageElementTag> namedRefs : namedRefTags.values()) {
      if ((namedRefs != null) && (namedRefs.size() > 1)) {

        // Check if there is an error
        boolean hardError = false;
        boolean softError = false;
        PageElementTag firstNamedRef = namedRefs.get(0);
        String firstValue = content.substring(
            firstNamedRef.getValueBeginIndex(),
            firstNamedRef.getValueEndIndex());
        for (PageElementTag namedRef : namedRefs) {
          String value = content.substring(namedRef.getValueBeginIndex(), namedRef.getValueEndIndex());
          if (!firstValue.trim().equals(value.trim())) {
            hardError = true;
          } else if (!firstValue.equals(value)) {
            softError = true;
          }
        }

        // Report error
        if (hardError || softError) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Report each reference
          boolean first = true;
          for (PageElementTag namedRef : namedRefs) {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                namedRef.getCompleteBeginIndex(),
                namedRef.getCompleteEndIndex(),
                first ? ErrorLevel.CORRECT : ErrorLevel.ERROR);
            String value = content.substring(namedRef.getValueBeginIndex(), namedRef.getValueEndIndex());
            if (!hardError && !value.equals(value.trim())) {
              String replacement =
                  content.substring(namedRef.getCompleteBeginIndex(), namedRef.getValueBeginIndex()) +
                  value.trim() +
                  content.substring(namedRef.getValueEndIndex(), namedRef.getCompleteEndIndex());
              errorResult.addReplacement(replacement, GT._("Trim text"), true);
            }
            errors.add(errorResult);
            first = false;
          }
        }
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
