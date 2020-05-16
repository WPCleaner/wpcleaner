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
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 511 of check wikipedia project.
 * Error 513: Internal link inside external link
 */
public class CheckErrorAlgorithm513 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm513() {
    super("Internal link inside external link");
  }

  private static final String PUNCTUATION = ",-:";

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
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    for (PageElementExternalLink link : links) {
      result |= analyzeExternalLink(link, analysis, errors);
    }

    return result;
  }

  /**
   * Analyze one external link.
   * 
   * @param link External link to analyze.
   * @param analysis Page analysis.
   * @param errors Errors found in the page
   * @return Flag indicating if an error was found in the external link.
   */
  private boolean analyzeExternalLink(
      PageElementExternalLink link,
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Criteria on the external link itself
    if (!link.hasSquare() ||
        (link.getText() == null) ||
        link.hasSecondSquare()) {
      return false;
    }

    // Criteria on the internal link inside the external link
    PageElementInternalLink internalLink = analysis.isInInternalLink(link.getEndIndex());
    if ((internalLink == null) ||
        (internalLink.getBeginIndex() != link.getEndIndex())) {
      return false;
    }

    // Report error immediately if no list of errors
    if (errors == null) {
      return true;
    }

    // Analyze contents for potential replacements
    String contents = analysis.getContents();
    int beginExtra = internalLink.getBeginIndex();
    int endExtra = internalLink.getEndIndex();
    boolean automatic = false;
    if ((endExtra < contents.length()) &&
        contents.startsWith("''", endExtra)) {
      int countQuote = 2;
      while ((endExtra + countQuote < contents.length()) &&
          (contents.charAt(endExtra + countQuote) == '\'')) {
        countQuote++;
      }
      if ((countQuote == 2) ||
          (countQuote == 3) ||
          (countQuote == 5)) {
        if ((beginExtra > countQuote) &&
            contents.substring(0, beginExtra).endsWith(contents.substring(endExtra, endExtra + countQuote))) {
          beginExtra -= countQuote;
          endExtra += countQuote;
        }
      }
    }
    while ((beginExtra > 0) &&
        CharacterUtils.isWhitespace(contents.charAt(beginExtra - 1))) {
      beginExtra--;
    }
    String prefix = contents.substring(link.getBeginIndex(), beginExtra);
    for (String text : textsBefore) {
      if (prefix.endsWith(text)) {
        beginExtra -= text.length();
        automatic = true;
      }
    }
    while ((beginExtra > 0) &&
        (CharacterUtils.isWhitespace(contents.charAt(beginExtra - 1)) ||
         (PUNCTUATION.indexOf(contents.charAt(beginExtra - 1)) >= 0))) {
      if (PUNCTUATION.indexOf(contents.charAt(beginExtra - 1)) >= 0) {
        automatic = true;
      }
      beginExtra--;
    }
    if (beginExtra <= link.getBeginIndex() + link.getTextOffset()) {
      automatic = false;
    }

    // Check for extra bracket at the end
    boolean closeBracket = false;
    int beginError = link.getBeginIndex();
    int endError = endExtra;
    while ((endError < contents.length()) &&
        (CharacterUtils.isWhitespace(contents.charAt(endError)) ||
         (".)".indexOf(contents.charAt(endError)) >= 0))) {
      endError++;
    }
    int tmpEnd = endError;
    while ((tmpEnd < contents.length()) &&
        ("\n[]{}".indexOf(contents.charAt(tmpEnd)) < 0)) {
      tmpEnd++;
    }
    if ((tmpEnd < contents.length()) &&
        (tmpEnd > endError) &&
        (contents.charAt(tmpEnd) == ']')) {
      endError = tmpEnd;
      automatic = false;
    }
    if ((endError < contents.length()) &&
        (contents.charAt(endError) == ']')) {
      closeBracket = true;
      endError++;
    }

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginError, endError);
    if (closeBracket) {
      String replacement =
          contents.substring(beginError, beginExtra) +
          "]" +
          contents.substring(beginExtra, endError - 1);
      errorResult.addReplacement(replacement, automatic);
    }
    errorResult.addReplacement(
        contents.substring(beginError, internalLink.getBeginIndex()) +
        internalLink.getDisplayedTextNotTrimmed() +
        contents.substring(internalLink.getEndIndex(), endError));
    errorResult.addPossibleAction(new SimpleAction(
        GT._T("External Viewer"),
        new ActionExternalViewer(link.getLink())));
    errors.add(errorResult);
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of potential texts that can be used after the internal link */
  private static final String PARAMETER_TEXTS_AFTER = "texts_after";

  /** List of potential texts that can be used before the internal link */
  private static final String PARAMETER_TEXTS_BEFORE = "texts_before";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEXTS_AFTER, true, true, false);
    textsAfter.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        textsAfter.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_TEXTS_BEFORE, true, true, false);
    textsBefore.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        textsBefore.addAll(tmpList);
      }
    }
  }

  /** Texts after the internal link */
  private final List<String> textsAfter = new ArrayList<>();

  /** Texts before the internal link */
  private final List<String> textsBefore = new ArrayList<>();

  /**
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        PARAMETER_TEXTS_BEFORE,
        GT._T("A list of texts that can be before the internal link"));
    return parameters;
  }
}
