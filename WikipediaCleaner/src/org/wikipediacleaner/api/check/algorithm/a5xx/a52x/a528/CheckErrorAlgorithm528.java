/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a52x.a528;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementPMID;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
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
        for (String[] pmidTemplate : pmidTemplates) {
          if (pmidTemplate.length > 2) {
            String templateName = pmidTemplate[0];
            String[] params = pmidTemplate[1].split(",");
            Boolean suggested = Boolean.valueOf(pmidTemplate[2]);
            if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
              TemplateBuilder builder = TemplateBuilder.from(templateName);
              builder.addParam(
                  !"1".equals(params[0]) ? params[0] : null,
                  pmid.getPMID());
              errorResult.addReplacement(builder.toString());
            }
          }
        }

        // Suggest replacement with interwikis
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
    if (categoryName != null) {
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
    String category = getTrackingCategory();
    if (category != null) {
      API api = APIFactory.getAPI();
      String title = wiki.getWikiConfiguration().getPageTitle(Namespace.CATEGORY, category);
      Page categoryPage = DataManager.createSimplePage(wiki, title, null, null, Namespace.CATEGORY);
      try {
        api.retrieveCategoryMembers(wiki, categoryPage, 0, false, limit);
        result = categoryPage.getRelatedPages(RelatedPages.CATEGORY_MEMBERS);
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

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Category containing the list of pages in error */
  private static final String PARAMETER_CATEGORY = "category";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_CATEGORY, true, true, false);
    categoryName = null;
    if ((tmp != null) &&
        (tmp.trim().length() > 0)) {
      categoryName = tmp.trim();
    }

    List<String[]> tmpList = getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.PMID_TEMPLATES);
    pmidTemplates.clear();
    if (tmpList != null) {
      for (String[] pmidTemplate : tmpList) {
        if (pmidTemplate.length > 2) {
          pmidTemplates.add(pmidTemplate);
        }
      }
    }

    tmpList = getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.PMID_INTERWIKIS);
    pmidInterwikis.clear();
    if (tmpList != null) {
      for (String[] pmidInterwiki : tmpList) {
        if (pmidInterwiki.length > 0) {
          pmidInterwikis.add(pmidInterwiki);
        }
      }
    }
  }

  /** Category containing the list of pages in error */
  private String categoryName = null;

  /** Templates for PMID */
  private List<String[]> pmidTemplates = new ArrayList<>();

  /** Interwikis for PMID */
  private List<String[]> pmidInterwikis = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_CATEGORY,
        GT._T("A category containing the list of pages in error"),
        new AlgorithmParameterElement(
            "category name",
            GT._T("A category containing the list of pages in error"))));
  }
}
