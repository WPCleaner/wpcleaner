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

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementRFC;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 530 of check wikipedia project.
 * Error 530: RFC magical link
 */
public class CheckErrorAlgorithm530 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm530() {
    super("RFC magical link");
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

    // Analyze each RFC
    boolean result = false;
    List<PageElementRFC> rfcs = analysis.getRFCs();
    if ((rfcs == null) || (rfcs.isEmpty())) {
      return false;
    }
    for (PageElementRFC rfc : rfcs) {
      boolean isError = false;
      if (!rfc.isTemplateParameter() && rfc.isCorrect()) {
        if (analysis.isInExternalLink(rfc.getBeginIndex()) == null) {
          isError = true;
        }
      }

      if (isError) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, rfc.getBeginIndex(), rfc.getEndIndex());

        // Suggest replacement with templates
        for (String[] rfcTemplate : rfcTemplates) {
          if (rfcTemplate.length > 2) {
            String templateName = rfcTemplate[0];
            String[] params = rfcTemplate[1].split(",");
            Boolean suggested = Boolean.valueOf(rfcTemplate[2]);
            if ((params.length > 0) && (Boolean.TRUE.equals(suggested))) {
              StringBuilder replacement = new StringBuilder();
              replacement.append("{{");
              replacement.append(templateName);
              replacement.append("|");
              if (!"1".equals(params[0])) {
                replacement.append(params[0]);
                replacement.append("=");
              }
              replacement.append(rfc.getRFC());
              replacement.append("}}");
              errorResult.addReplacement(replacement.toString());
            }
          }
        }

        // Suggest replacement with interwikis
        for (String[] rfcInterwiki : rfcInterwikis) {
          if (rfcInterwiki.length > 0) {
            String rfcCode = rfcInterwiki[0];
            StringBuilder replacement = new StringBuilder();
            replacement.append("[[:");
            replacement.append(rfcCode);
            replacement.append(":");
            replacement.append(rfc.getRFC());
            replacement.append("|");
            replacement.append(PageElementRFC.RFC_PREFIX);
            replacement.append(" ");
            replacement.append(rfc.getRFC());
            replacement.append("]]");
            errorResult.addReplacement(replacement.toString());
          }
        }

        // Suggest to view the RFC
        errorResult.addPossibleAction(new SimpleAction(
            GT._T("View RFC"),
            new ActionExternalViewer(rfc.getURL())));

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
      Page categoryPage = DataManager.getPage(wiki, title, null, null, null);
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
        WPCConfigurationStringList.RFC_TEMPLATES);
    rfcTemplates.clear();
    if (tmpList != null) {
      for (String[] rfcTemplate : tmpList) {
        if (rfcTemplate.length > 2) {
          rfcTemplates.add(rfcTemplate);
        }
      }
    }

    tmpList = getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.RFC_INTERWIKIS);
    rfcInterwikis.clear();
    if (tmpList != null) {
      for (String[] rfcInterwiki : tmpList) {
        if (rfcInterwiki.length > 0) {
          rfcInterwikis.add(rfcInterwiki);
        }
      }
    }
  }

  /** Category containing the list of pages in error */
  private String categoryName = null;

  /** Templates for RFC */
  private List<String[]> rfcTemplates = new ArrayList<>();

  /** Interwikis for RFC */
  private List<String[]> rfcInterwikis = new ArrayList<>();

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
