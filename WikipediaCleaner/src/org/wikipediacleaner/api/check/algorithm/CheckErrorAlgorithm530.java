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
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.Page.RelatedPages;
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
        List<String[]> rfcTemplates = analysis.getWPCConfiguration().getStringArrayList(
            WPCConfigurationStringList.RFC_TEMPLATES);
        if (rfcTemplates != null) {
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
        }

        // Suggest replacement with interwikis
        List<String[]> rfcInterwikis = analysis.getWPCConfiguration().getStringArrayList(
            WPCConfigurationStringList.RFC_INTERWIKIS);
        if (rfcInterwikis != null) {
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
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("category", GT._T("A category containing the list of pages in error"));
    return parameters;
  }
}
