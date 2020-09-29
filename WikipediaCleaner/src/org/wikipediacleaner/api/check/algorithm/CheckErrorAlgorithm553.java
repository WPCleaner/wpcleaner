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
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 553 of check wikipedia project.
 * Error 553: nowiki after internal link.
 */
public class CheckErrorAlgorithm553 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm553() {
    super("nowiki after internal link");
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
    List<PageElementTag> nowikiTags = analysis.getTags(PageElementTag.TAG_WIKI_NOWIKI);
    if ((nowikiTags == null) || (nowikiTags.isEmpty())) {
      return false;
    }

    // Check each nowiki tag
    boolean result = false;
    for (PageElementTag nowikiTag : nowikiTags) {
      result |= analyzeTag(analysis, errors, nowikiTag);
    }

    return result;
  }

  /**
   * Analyze a nowiki tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param nowikiTag Nowiki tag to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag nowikiTag) {

    // Only work on full tags
    if ((nowikiTag == null) || !nowikiTag.isFullTag()) {
      return false;
    }

    // Check if there's an internal link before the nowiki tag
    int endText = nowikiTag.getBeginIndex();
    String contents = analysis.getContents();
    if ((endText <= 0) || (contents.charAt(endText - 1) !=  ']')) {
      return false;
    }
    PageElementInternalLink link = analysis.isInInternalLink(endText - 1);
    if ((link == null) || (link.getEndIndex() != endText)) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Extend selection
    endText = nowikiTag.getEndIndex();
    boolean extraCharacters = false;
    while ((endText < contents.length()) &&
        Character.isLetter(contents.charAt(endText))) {
      extraCharacters = true;
      endText++;
    }

    // Check if it's safe after the selection
    int endIndex = endText;
    boolean safeEnd = false;
    if (endIndex < contents.length()) {
      char lastChar = contents.charAt(endIndex);
      if (CharacterUtils.isWhitespace(lastChar) ||
          CharacterUtils.isPunctuation(lastChar) ||
          ("'".indexOf(lastChar) >= 0)) {
        safeEnd = true;
      } else {
        endIndex++;
      }
    } else {
      safeEnd = true;
    }

    // Report error
    int beginIndex = link.getBeginIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    if (!extraCharacters) {
      String replacement =
          contents.substring(beginIndex, link.getEndIndex()) +
          contents.substring(nowikiTag.getEndIndex(), endIndex);
      errorResult.addReplacement(replacement, safeEnd);
    } else {
      String extraText = contents.substring(nowikiTag.getEndIndex(), endText);
      boolean automatic = suffixes.contains(extraText);
      if (link.getText() == null) {
        String replacement =
            contents.substring(beginIndex, nowikiTag.getBeginIndex()) +
            contents.substring(nowikiTag.getEndIndex(), endIndex);
        errorResult.addReplacement(replacement, automatic);
      } else {
        String fullLink = link.getFullLink();
        String text = link.getDisplayedText() + extraText;
        String replacement =
            PageElementInternalLink.createInternalLink(fullLink, text) +
            contents.substring(endText, endIndex);
        boolean safeLink = isSafeLink(link, text);
        errorResult.addReplacement(replacement, automatic || safeLink);
        if (!automatic && !safeLink) {
          errorResult.addReplacement(text + contents.substring(endText, endIndex));
        }
      }
      if (!automatic) {
        String linkText = contents.substring(beginIndex, nowikiTag.getBeginIndex());
        char lastChar = contents.charAt(link.getEndIndex() - 3);
        if (" \u00A0;".indexOf(lastChar) >= 0) {
          linkText =
              contents.substring(beginIndex, link.getEndIndex() - 3) +
              contents.substring(link.getEndIndex() - 2, nowikiTag.getBeginIndex());
          automatic = true;
        }
        String replacement =
            linkText + " " +
            contents.substring(nowikiTag.getEndIndex(), endIndex);
        errorResult.addReplacement(replacement, automatic);
      }
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * Tells if it is safe to make a link.
   * 
   * @param link Link.
   * @param text Suggested text for the link.
   * @return True if it is safe to make a link.
   */
  private boolean isSafeLink(PageElementInternalLink link, String text) {
    if (isSafeLink(link.getFullLink(), text)) {
      return true;
    }
    if (link.getAnchor() != null) {
      if (isSafeLink(link.getLink(), text)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Tells if it is safe to make a link.
   * 
   * @param link Link target.
   * @param text Suggested text for the link.
   * @return True if it is safe to make a link.
   */
  private boolean isSafeLink(String link, String text) {
    if ((link == null) || (text == null)) {
      return false;
    }
    if (Page.areSameTitle(link, text)) {
      return true;
    }
    if (Page.areSameTitle(
        link.toUpperCase().replaceAll("-", " "),
        text.toUpperCase().replaceAll("-", " "))) {
      return true;
    }
    int parenthesis = link.indexOf('(');
    if ((parenthesis > 0) && (isSafeLink(link.substring(0, parenthesis), text))) {
      return true;
    }
    int comma = link.indexOf(',');
    if ((comma > 0) && (isSafeLink(link.substring(0, comma), text))) {
      return true;
    }
    return false;
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

  /** List of suffixes to accept automatically */
  private static final String PARAMETER_SUFFIXES = "suffixes";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_SUFFIXES, true, true, true);
    suffixes.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        suffixes.addAll(tmpList);
      }
    }
  }

  /** Suffixes to accept automatically */
  private final Set<String> suffixes = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_SUFFIXES,
        GT._T("Acceptable suffixes"),
        new AlgorithmParameterElement(
            "suffix",
            GT._T("Acceptable suffix")),
        true));
  }
}
