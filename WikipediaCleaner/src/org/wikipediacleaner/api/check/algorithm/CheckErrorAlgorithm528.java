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
import org.wikipediacleaner.api.data.PageElementPMID;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 528 of check wikipedia project.
 * Error 528: PMID magical link
 */
public class CheckErrorAlgorithm528 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm528() {
    super("PMID magical link");
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
    List<PageElementPMID> pmids = analysis.getPMIDs();
    if ((pmids == null) || (pmids.isEmpty())) {
      return false;
    }
    for (PageElementPMID pmid : pmids) {
      boolean isError = false;
      if (!pmid.isTemplateParameter() && pmid.isCorrect()) {
        if (analysis.isInExternalLink(pmid.getBeginIndex()) == null) {
          isError = true;
        }
      }

      if (isError) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, pmid.getBeginIndex(), pmid.getEndIndex());

        // Suggest replacement with templates
        List<String[]> pmidTemplates = analysis.getWPCConfiguration().getStringArrayList(
            WPCConfigurationStringList.PMID_TEMPLATES);
        if (pmidTemplates != null) {
          for (String[] pmidTemplate : pmidTemplates) {
            if (pmidTemplate.length > 2) {
              String templateName = pmidTemplate[0];
              String[] params = pmidTemplate[1].split(",");
              Boolean suggested = Boolean.valueOf(pmidTemplate[2]);
              if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
                StringBuilder replacement = new StringBuilder();
                replacement.append("{{");
                replacement.append(templateName);
                replacement.append("|");
                if (!"1".equals(params[0])) {
                  replacement.append(params[0]);
                  replacement.append("=");
                }
                replacement.append(pmid.getPMID());
                replacement.append("}}");
                errorResult.addReplacement(replacement.toString());
              }
            }
          }
        }

        // Suggest replacement with interwikis
        List<String[]> pmidInterwikis = analysis.getWPCConfiguration().getStringArrayList(
            WPCConfigurationStringList.PMID_INTERWIKIS);
        if (pmidInterwikis != null) {
          for (String[] pmidInterwiki : pmidInterwikis) {
            if (pmidInterwiki.length > 0) {
              String pmidCode = pmidInterwiki[0];
              StringBuilder replacement = new StringBuilder();
              replacement.append("[[:");
              replacement.append(pmidCode);
              replacement.append(":");
              replacement.append(pmid.getPMID());
              replacement.append("|");
              replacement.append(PageElementPMID.PMID_PREFIX);
              replacement.append(" ");
              replacement.append(pmid.getPMID());
              replacement.append("]]");
              errorResult.addReplacement(replacement.toString());
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
