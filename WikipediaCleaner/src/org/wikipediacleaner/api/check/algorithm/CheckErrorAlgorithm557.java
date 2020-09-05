/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 557 of check wikipedia project.
 * Error 557: missing space before internal link.
 */
public class CheckErrorAlgorithm557 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm557() {
    super("missing space before internal link");
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

    // Global verification
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    if ((analysis.getPage().getRedirects() != null) &&
        analysis.getPage().getRedirects().isRedirect()) {
      return false;
    }

    // Check each internal link
    boolean result = false;
    for (PageElementInternalLink link : links) {
      result |= analyzeLink(analysis, errors, link);
    }

    return result;
  }

  /**
   * Analyze an internal link to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Check the character before the link
    int beginIndex = link.getBeginIndex();
    if (beginIndex == 0) {
      return false;
    }
    String contents = analysis.getContents();
    char previousChar = contents.charAt(beginIndex - 1);
    if (!Character.isLetter(previousChar)) {
      return false;
    }

    // Check if this is an accepted prefix
    if (!prefixes.isEmpty()) {
      int tmpIndex = beginIndex - 1;
      while ((tmpIndex > 0) &&
          Character.isLetter(contents.charAt(tmpIndex - 1))) {
        tmpIndex--;
      }
      if (prefixes.contains(contents.substring(tmpIndex, beginIndex).toUpperCase())) {
        return false;
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    while ((beginIndex > 0) &&
        Character.isLetter(contents.charAt(beginIndex - 1))) {
      beginIndex--;
    }
    int endIndex = link.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    String replacement =
        contents.substring(beginIndex, link.getBeginIndex()) +
        " " +
            contents.substring(link.getBeginIndex(), link.getEndIndex());
    errorResult.addReplacement(replacement);
    if ((beginIndex <= 0) ||
        Character.isWhitespace(contents.charAt(beginIndex - 1))) {
      replacement = PageElementInternalLink.createInternalLink(
          link.getLink(),
          link.getAnchor(),
          contents.substring(beginIndex, link.getBeginIndex()) + link.getDisplayedText());
      errorResult.addReplacement(replacement);
    }
    errors.add(errorResult);
    return true;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of possible prefixes */
  private static final String PARAMETER_PREFIXES = "prefixes";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_PREFIXES, true, true, true);
    prefixes.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpElement : tmpList) {
          prefixes.add(tmpElement.toUpperCase());
        }
      }
    }
  }

  /** Prefixes that can be before an internal link */
  private final Set<String> prefixes = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_PREFIXES,
        GT._T("Prefixes which can be before an internal link"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "prefix",
              GT._T("Prefix which can be before an internal link"))
        },
        true));
  }
}
