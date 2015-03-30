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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Algorithm for analyzing error 526 of check wikipedia project.
 * Error 526: Incorrect date link
 */
public class CheckErrorAlgorithm526 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm526() {
    super("Incorrect date link");
  }

  /** Minimum length of the year */
  private static final int MIN_LENGTH = 3;

  /** Maximum length of the year */
  private static final int MAX_LENGTH = 4;

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each internal link
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if ((links == null) || links.isEmpty()) {
      return false;
    }
    boolean result = false;
    for (PageElementInternalLink link : links) {

      // Decide if link is an error
      String target = link.getLink();
      String text = link.getText();
      boolean isProblematic = false;
      if ((target != null) &&
          (text != null) &&
          !target.equals(text) &&
          (target.length() >= MIN_LENGTH) &&
          (target.length() <= MAX_LENGTH) &&
          (text.length() >= MIN_LENGTH) &&
          (text.length() <= MAX_LENGTH)) {
        isProblematic = true;
        int pos = 0;
        while (isProblematic && (pos < target.length())) {
          if (!Character.isDigit(target.charAt(pos))) {
            isProblematic = false;
          }
          pos++;
        }
        pos = 0;
        while (isProblematic && (pos < text.length())) {
          if (!Character.isDigit(text.charAt(pos))) {
            isProblematic = false;
          }
          pos++;
        }
      }

      // Report error
      if (isProblematic) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        errorResult.addReplacement(PageElementInternalLink.createInternalLink(target, target));
        errorResult.addReplacement(PageElementInternalLink.createInternalLink(text, text));
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
    return (getAbuseFilter() != null);
  }

  /**
   * @return Abuse filter.
   */
  private Integer getAbuseFilter() {
    String abuseFilter = getSpecificProperty("abuse_filter", true, true, false);
    if ((abuseFilter != null) &&
        (abuseFilter.trim().length() > 0)) {
      try {
        return Integer.valueOf(abuseFilter);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
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
    Integer abuseFilter = getAbuseFilter();
    if (abuseFilter != null) {
      API api = APIFactory.getAPI();
      Configuration config = Configuration.getConfiguration();
      int maxDays = config.getInt(wiki, ConfigurationValueInteger.MAX_DAYS_ABUSE_LOG);
      try {
        result = api.retrieveAbuseLog(wiki, abuseFilter, maxDays);
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "abuse_filter",
        GT._("An identifier of an abuse filter that is triggered by incorrect year links."));
    return parameters;
  }
}
