/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.lang.Character.UnicodeBlock;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
    UnicodeBlock unicodeBlock = UnicodeBlock.of(previousChar);
    if (unicodeBlock != null) {
      if (UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS.equals(unicodeBlock)) {
        return false;
      }
    }

    // Check if this is an accepted prefix
    if (!prefixes_ok.isEmpty()) {
      int tmpIndex = beginIndex - 1;
      while ((tmpIndex > 0) &&
          Character.isLetter(contents.charAt(tmpIndex - 1))) {
        tmpIndex--;
      }
      if (prefixes_ok.contains(contents.substring(tmpIndex, beginIndex).toUpperCase())) {
        return false;
      }
    }

    // Check if the error should be ignored
    if (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_TIMELINE, beginIndex) != null) {
      return false;
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
    String displayedText = link.getDisplayedTextNotTrimmed();
    String prefix = contents.substring(beginIndex, link.getBeginIndex());
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);

    String replacement = null;
    if ((displayedText.length() > 0) &&
        (" \u00A0;'’".indexOf(displayedText.charAt(0)) >= 0)) {

      // Decide which first char should be used
      char firstChar = displayedText.charAt(0);
      if (firstChar == 0xA0) {
        firstChar = ' ';
      }

      if (displayedText.length() > 1) {

        // Move the white space or apostrophe before the internal link
        replacement =
            prefix + firstChar +
            PageElementInternalLink.createInternalLink(
                link.getLink(),
                link.getAnchor(),
                displayedText.substring(1));
        boolean automatic = true;
        if (firstChar == '\'') {
          if (displayedText.startsWith("'",  1)) {
            automatic = false;
          }
          if (!link.getLink().isEmpty() &&
              link.getLink().startsWith("'")) {
            automatic = false;
          }
        }
        if ((firstChar == '\'') || (firstChar == '’')) {
          // Ignore "'s "
          if (displayedText.startsWith("s ", 1)) {
            automatic = false;
          }
        }
        errorResult.addReplacement(replacement, automatic);

        // Add a white space before the internal link
        if (!automatic) {
          replacement =
              prefix + " " +
              contents.substring(link.getBeginIndex(), link.getEndIndex());
          errorResult.addReplacement(replacement, prefixes_exclude.contains(prefix));
        }
      } else {
        replacement = prefix + displayedText;
        errorResult.addReplacement(replacement);
      }

    } else {

      // Include the text before the internal link
      replacement = PageElementInternalLink.createInternalLink(
          link.getLink(),
          link.getAnchor(),
          prefix + displayedText);
      errorResult.addReplacement(
          replacement,
          areIdentical(link.getLink(), prefix + displayedText, true));

      // Extract the beginning of the internal link
      int spaceIndex = displayedText.indexOf(' ');
      while ((spaceIndex > 0) && (spaceIndex < displayedText.length() - 1)) {
        String fullPrefix = prefix + displayedText.substring(0, spaceIndex + 1);
        replacement =
            fullPrefix +
            PageElementInternalLink.createInternalLink(
                link.getLink(),
                link.getAnchor(),
                displayedText.substring(spaceIndex + 1));
        boolean automatic = areIdentical(link.getLink(), displayedText.substring(spaceIndex), true);
        automatic &= (spaceIndex <= 2) || (prefixes_exclude.contains(fullPrefix.trim()));
        errorResult.addReplacement(
            replacement,
            automatic);
        spaceIndex = displayedText.indexOf(' ', spaceIndex + 1);
      }

      // Add a white space before the internal link
      replacement =
          prefix + " " +
          contents.substring(link.getBeginIndex(), link.getEndIndex());
      errorResult.addReplacement(replacement, prefixes_exclude.contains(prefix));

      // Add other separators before the internal link
      for (String separator : separators) {
        replacement =
            prefix + separator +
            contents.substring(link.getBeginIndex(), link.getEndIndex());
        errorResult.addReplacement(replacement);
      }

      // Include the prefix in the link target
      if (StringUtils.isEmpty(link.getText())) {
        replacement = PageElementInternalLink.createInternalLink(
            prefix + displayedText,
            link.getAnchor(),
            prefix + displayedText);
        errorResult.addReplacement(replacement);
      }

      // Remove the prefix
      replacement = contents.substring(link.getBeginIndex(), link.getEndIndex());
      errorResult.addReplacement(replacement);
    }

    // Remove internal link
    errorResult.addReplacement(prefix + link.getDisplayedText());

    // View internal link
    errorResult.addPossibleAction(
        GT._T("External Viewer"),
        new BasicActionProvider(
            new ActionExternalViewer(analysis.getWikipedia(), link.getLink())));

    errors.add(errorResult);
    return true;
  }

  /**
   * @param link Link target.
   * @param text Text.
   * @return True if link and text can be considered identical.
   */
  private boolean areIdentical(String link, String text, boolean extraTrim) {
    link = link.trim().toUpperCase();
    text = text.trim().toUpperCase();
    if (Page.areSameTitle(link, text)) {
      return true;
    }
    if (link.endsWith(")") && extraTrim) {
      int openParenthesis = link.lastIndexOf('(');
      if (openParenthesis > 0) {
        return areIdentical(link.substring(0,  openParenthesis), text, extraTrim);
      }
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

  /** List of possible prefixes */
  private static final String PARAMETER_PREFIXES_OK = "prefixes_ok";

  /** List of prefixes to exclude from the link */
  private static final String PARAMETER_PREFIXES_EXCLUDE = "prefixes_exclude";

  /** List of potential separators between the text and the link */
  private static final String PARAMETER_SEPARATORS = "separators";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_PREFIXES_OK, true, true, true);
    prefixes_ok.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpElement : tmpList) {
          prefixes_ok.add(tmpElement.toUpperCase());
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_PREFIXES_EXCLUDE, true, true, true);
    prefixes_exclude.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        prefixes_exclude.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_SEPARATORS, true, true, true);
    separators.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        separators.addAll(tmpList);
      }
    }
  }

  /** Prefixes that can be before an internal link */
  private final Set<String> prefixes_ok = new HashSet<>();

  /** Prefixes that should be excluded from the internal link */
  private final Set<String> prefixes_exclude = new HashSet<>();

  /** List of potential separators between the text and the link */
  private final List<String> separators = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_PREFIXES_OK,
        GT._T("Prefixes which can be before an internal link"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "prefix",
              GT._T("Prefix which can be before an internal link"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_PREFIXES_EXCLUDE,
        GT._T("Prefixes which should be excluded from the internal link"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "prefix",
              GT._T("Prefix which should be excluded from the internal link"))
        },
        true));
  }
}
