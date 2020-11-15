/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.ContentsInternalLinkBuilder;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Algorithm for analyzing error 526 of check wikipedia project.
 * Error 526: Incorrect link
 */
public class CheckErrorAlgorithm526 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm526() {
    super("Incorrect date link");
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
      result |= analyzeInternalLink(analysis, errors, link);
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Basic check on the link
    String target = link.getFullLink();
    String text = link.getText();
    if ((target == null) ||
        (text == null) ||
        Page.areSameTitle(target, text)) {
      return false;
    }
    if ((text.length() < minLength) ||
        (text.length() > maxLength)) {
      return false;
    }

    // Check text first (only digits)
    for (int pos = 0; pos < text.length(); pos++) {
      if (!Character.isDigit(text.charAt(pos))) {
        return false;
      }
    }
    int yearDisplayed = Integer.parseInt(text);
    if (yearDisplayed <= 0) {
      return false;
    }

    // Check link
    int nbDigits = 0;
    while ((nbDigits < target.length()) &&
           (Character.isDigit(target.charAt(nbDigits)))) {
      nbDigits++;
    }
    if ((nbDigits < minLength) ||
        (nbDigits > maxLength)) {
      return false;
    }

    // Compare values
    int yearLinked = Integer.parseInt(target.substring(0, nbDigits));
    if (yearLinked <= 0) {
      return false;
    }
    if ((yearDisplayed < yearLinked + minAbove) &&
        (yearDisplayed > yearLinked - minBelow)) {
      return false;
    }

    // Check extra characters in the link
    if (target.length() > nbDigits) {
      if (!CharacterUtils.isWhitespace(target.charAt(nbDigits))) {
        return false;
      }
      for (int pos = nbDigits + 1; pos < target.length(); pos++) {
        if (Character.isDigit(target.charAt(pos))) {
          return false;
        }
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }

    // Extend link with extra text
    String contents = analysis.getContents();
    int endIndex = link.getEndIndex();
    while ((endIndex < contents.length()) &&
        Character.isLetterOrDigit(contents.charAt(endIndex))) {
      endIndex++;
    }
    String extraText = contents.substring(link.getEndIndex(), endIndex);

    // Create error
    ErrorLevel errorLevel = ErrorLevel.ERROR;
    if ((link.getEndIndex() < contents.length()) &&
        (contents.charAt(link.getEndIndex()) == '{')) {
      errorLevel = ErrorLevel.WARNING;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, link.getBeginIndex(), endIndex, errorLevel);
    if (!extraText.isEmpty()) {
      try {
        int yearCorrected = Integer.parseInt(text + extraText);
        if (yearLinked == yearCorrected) {
          errorResult.addReplacement(
              ContentsInternalLinkBuilder.from(target).withText(text + extraText).toString(),
              true);
        }
      } catch (NumberFormatException e) {
        // Nothing to do, it's just not an integer
      }
    }
    errorResult.addReplacement(
        ContentsInternalLinkBuilder.from(target).withText(target).toString() +
        extraText);
    errorResult.addReplacement(
        ContentsInternalLinkBuilder.from(text).withText(text).toString() +
        extraText);
    if (!askHelpList.isEmpty()) {
      boolean firstReplacement = true;
      for (String askHelpElement : askHelpList) {
        int pipeIndex = askHelpElement.indexOf('|');
        if ((pipeIndex > 0) && (pipeIndex < askHelpElement.length())) {
          String suffix = askHelpElement.substring(pipeIndex + 1);
          boolean botReplace = false;
          Page page = analysis.getPage();
          if (page.isArticle() && page.isInMainNamespace() &&
              suffix.startsWith("{{") &&
              (link.getEndIndex() < contents.length())) {
            char nextChar = contents.charAt(endIndex);
            if (nextChar != '{') {
              if ((target.indexOf('#') < 0) &&
                  (target.indexOf('(') < 0) &&
                  (target.indexOf(')') < 0) &&
                  (extraText.isEmpty())) {
                botReplace = true;
              }
            }
          }
          String replacement =
              analysis.getContents().substring(link.getBeginIndex(), endIndex) +
              suffix;
          errorResult.addReplacement(
              replacement,
              askHelpElement.substring(0, pipeIndex),
              false, firstReplacement && botReplace);
          firstReplacement = false;
        }
      }
    }
    errors.add(errorResult);

    return true;
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (abuseFilter != null) || (dumpAnalysis != null);
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
    List<Page> result = new ArrayList<>();

    // Use abuse filter
    if (abuseFilter != null) {
      API api = APIFactory.getAPI();
      Configuration config = Configuration.getConfiguration();
      int maxDays = config.getInt(wiki, ConfigurationValueInteger.MAX_DAYS_ABUSE_LOG);
      try {
        List<Page> tmpResult = api.retrieveAbuseLog(wiki, abuseFilter, maxDays);
        if (tmpResult != null) {
          result.addAll(tmpResult);
        }
      } catch (APIException e) {
        //
      }
    }

    // Use internal links
    if (dumpAnalysis != null) {
      API api = APIFactory.getAPI();
      Page page = DataManager.getPage(wiki, dumpAnalysis, null, null, null);
      try {
        api.retrieveLinks(wiki, page, null, null, false, false);
        if (page.getLinks() != null) {
          result.addAll(page.getLinks());
        }
      } catch (APIException e) {
        //
      }
    }

    Collections.sort(result);

    // Limit result size
    while (result.size() > limit) {
      result.remove(result.size() - 1);
    }

    return result;
  }

  /**
   * @param analysis Page analysis
   * @return Modified page content after bot fixing.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#internalBotFix(org.wikipediacleaner.api.data.analysis.PageAnalysis)
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    return fixUsingAutomaticBotReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Identifier of abuse filter */
  private static final String PARAMETER_ABUSE_FILTER = "abuse_filter";

  /** Text to ask for help */
  private static final String PARAMETER_ASK_HELP = "ask_help";

  /** Page containing a dump analysis of the error */
  private static final String PARAMETER_DUMP_ANALYSIS = "dump_analysis";

  /** Minimum length of the number to be reported */
  private static final String PARAMETER_MIN_LENGTH = "min_length";

  /** Maximum length of the number to be reported */
  private static final String PARAMETER_MAX_LENGTH = "max_length";

  /** Minimum difference below to report the link  */
  private static final String PARAMETER_MIN_BELOW = "min_below";

  /** Minimum difference above to report the link */
  private static final String PARAMETER_MIN_ABOVE = "min_above";

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

    tmp = getSpecificProperty(PARAMETER_ASK_HELP, true, true, false);
    askHelpList.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        askHelpList.addAll(tmpList);
      }
    }

    dumpAnalysis = getSpecificProperty(PARAMETER_DUMP_ANALYSIS, true, true, false);

    minLength = 3;
    try {
      tmp = getSpecificProperty(PARAMETER_MIN_LENGTH, true, true, false);
      if (tmp != null) {
        minLength = Integer.parseInt(tmp);
      }
    } catch (NumberFormatException e) {
      //
    }

    maxLength = 4;
    try {
      tmp = getSpecificProperty(PARAMETER_MAX_LENGTH, true, true, false);
      if (tmp != null) {
        maxLength = Integer.parseInt(tmp);
      }
    } catch (NumberFormatException e) {
      //
    }

    minBelow = 1;
    try {
      tmp = getSpecificProperty(PARAMETER_MIN_BELOW, true, true, false);
      if (tmp != null) {
        minBelow = Integer.parseInt(tmp);
      }
    } catch (NumberFormatException e) {
      //
    }

    minAbove = 1;
    try {
      tmp = getSpecificProperty(PARAMETER_MIN_ABOVE, true, true, false);
      if (tmp != null) {
        minAbove = Integer.parseInt(tmp);
      }
    } catch (NumberFormatException e) {
      //
    }
  }

  /** Identifier of abuse filter */
  private Integer abuseFilter = null;

  /** Texts to ask for help */
  private final List<String> askHelpList = new ArrayList<>();

  /** Page containing a dump analysis */
  private String dumpAnalysis = null;

  /** Minimum length of the number to be reported */
  private int minLength = 3;

  /** Maximum length of the number to be reported */
  private int maxLength = 4;

  /** Minimum difference below to report the link  */
  private int minBelow = 1;

  /** Minimum difference above to report the link */
  private int minAbove = 1;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_ABUSE_FILTER,
        GT._T("An identifier of an abuse filter that is triggered by incorrect year links."),
        new AlgorithmParameterElement(
            "abuse filter identifier",
            GT._T("An identifier of an abuse filter that is triggered by incorrect year links."))));
    addParameter(new AlgorithmParameter(
        PARAMETER_ASK_HELP,
        GT._T("Text added after the link to ask for help."),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "explanation",
                GT._T("Description of the action")),
            new AlgorithmParameterElement(
                "text",
                GT._T("Text added after the link to ask for help."))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_DUMP_ANALYSIS,
        GT._T("A page containing a dump analysis for this error."),
        new AlgorithmParameterElement(
            "page name",
            GT._T("A page containing a dump analysis for this error."))));
    addParameter(new AlgorithmParameter(
        PARAMETER_MIN_LENGTH,
        GT._T("Minimum number of digits for the year"),
        new AlgorithmParameterElement(
            "length",
            GT._T("Minimum number of digits for the year"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_MAX_LENGTH,
        GT._T("Maximum number of digits for the year"),
        new AlgorithmParameterElement(
            "length",
            GT._T("Maximum number of digits for the year"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_MIN_BELOW,
        GT._T("Minimum difference between the year displayed and the year linked"),
        new AlgorithmParameterElement(
            "difference",
            GT._T("Minimum difference between the year displayed and the year linked"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_MIN_ABOVE,
        GT._T("Minimum difference between the year linked and the year displayed"),
        new AlgorithmParameterElement(
            "length",
            GT._T("Minimum difference between the year linked and the year displayed"))));
  }
}
