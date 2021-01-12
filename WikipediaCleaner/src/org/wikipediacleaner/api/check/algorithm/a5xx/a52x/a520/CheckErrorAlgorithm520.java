/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a52x.a520;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Algorithm for analyzing error 520 of check wikipedia project.
 * Error 520: Weird characters (pawns, snowmen) in main namespace
 */
public class CheckErrorAlgorithm520 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm520() {
    super("Weird characters");
  }

  /**
   * Weird characters to look for.
   */
  private final static String weirdCharacters = "☃"; // pawns ♙, snowmen

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
    Integer ns = analysis.getPage().getNamespace();
    if ((ns == null) || (ns.intValue() != Namespace.MAIN)) {
      return false;
    }

    // Search weird characters
    String contents = analysis.getContents();
    boolean result = false;
    for (int index = 0; index < contents.length(); index++) {
      if (weirdCharacters.indexOf(contents.charAt(index)) >= 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, index, index + 1);
        errorResult.addReplacement("");
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
    return (abuseFilter != null);
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
    if (abuseFilter == null) {
      return null;
    }
    API api = APIFactory.getAPI();
    Configuration config = Configuration.getConfiguration();
    int maxDays = config.getInt(wiki, ConfigurationValueInteger.MAX_DAYS_ABUSE_LOG);
    try {
      return api.retrieveAbuseLog(wiki, abuseFilter, maxDays);
    } catch (APIException e) {
      //
    }
    return null;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Identifier of abuse filter */
  private static final String PARAMETER_ABUSE_FILTER = "abuse_filter";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_ABUSE_FILTER, true, true, false);
    abuseFilter = null;
    if ((tmp != null) && (tmp.trim().length() > 0)) {
      try {
        abuseFilter = Integer.valueOf(tmp);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
    }
  }

  /** Identifier of abuse filter */
  private Integer abuseFilter = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_ABUSE_FILTER,
        GT._T("An identifier of an abuse filter that is triggered by weird characters."),
        new AlgorithmParameterElement(
            "abuse filter identifier",
            GT._T("An identifier of an abuse filter that is triggered by weird characters."))));
  }
}
