/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a55x.a557;

import java.lang.Character.UnicodeBlock;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Nonnull;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
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
    if ("ʼʹ".indexOf(previousChar) >= 0) {
      return false;
    }
    if ((link.getText() == null) &&
        (link.getLink() != null) &&
        (link.getLink().startsWith("."))) {
      return false;
    }
    UnicodeBlock unicodeBlock = UnicodeBlock.of(previousChar);
    if (unicodeBlock != null) {
      if (UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS.equals(unicodeBlock) ||
          UnicodeBlock.GREEK.equals(unicodeBlock) ||
          UnicodeBlock.HIRAGANA.equals(unicodeBlock)) {
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
    if (analysis.getSurroundingTag(WikiTagType.TIMELINE, beginIndex) != null) {
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
    boolean automaticUsed = displayedText.isEmpty() || CharacterUtils.isPunctuation(displayedText.charAt(0));
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
            InternalLinkBuilder
                .from(link.getLink())
                .withAnchor(link.getAnchor())
                .withText(displayedText.substring(1))
                .toString();
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
        errorResult.addReplacement(replacement, !automaticUsed && automatic);
        automaticUsed |= automatic;

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
      EnumWikipedia wiki = analysis.getWikipedia();
      replacement = InternalLinkBuilder
          .from(link.getLink())
          .withAnchor(link.getAnchor())
          .withText(prefix + displayedText)
          .toString();
      boolean automatic = isSafeLink(link, prefix + displayedText, wiki);
      errorResult.addReplacement(
          replacement,
          !automaticUsed && automatic);
      automaticUsed |= automatic;

      // Extract the beginning of the internal link
      int spaceIndex = displayedText.indexOf(' ');
      while ((spaceIndex > 0) && (spaceIndex < displayedText.length() - 1)) {
        String fullPrefix = prefix + displayedText.substring(0, spaceIndex + 1);
        String text = displayedText.substring(spaceIndex + 1);
        replacement =
            fullPrefix +
            InternalLinkBuilder
                .from(link.getLink())
                .withAnchor(link.getAnchor())
                .withText(text)
                .toString();
        automatic = false;
        if (link.getText() != null) {
          automatic = isSafeLink(link, text, wiki);
          if (automatic &&
              (beginIndex > 0) &&
              (analysis.isInInternalLink(beginIndex - 1) != null)) {
            automatic = false;
          }
        } else {
          automatic = areIdentical(link.getLink(), text, wiki);
        }
        automatic &=
            (fullPrefix.length() <= 3) ||
            (prefixes_exclude.contains(fullPrefix.trim()));
        errorResult.addReplacement(
            replacement,
            !automaticUsed && automatic);
        automaticUsed |= automatic;
        spaceIndex = displayedText.indexOf(' ', spaceIndex + 1);
      }

      // Add a white space before the internal link
      replacement =
          prefix + " " +
          contents.substring(link.getBeginIndex(), link.getEndIndex());
      automatic = prefixes_exclude.contains(prefix);
      // Can't use areIdentical(link.getLink(), link.getDisplayedText(), true);
      // because prefix could also be included in the link in some cases
      errorResult.addReplacement(
          replacement,
          !automaticUsed && automatic);
      automaticUsed |= automatic;

      // Add other separators before the internal link
      for (String separator : separators) {
        replacement =
            prefix + separator +
            contents.substring(link.getBeginIndex(), link.getEndIndex());
        errorResult.addReplacement(replacement);
      }

      // Include the prefix in the link target
      if (StringUtils.isEmpty(link.getText())) {
        replacement = InternalLinkBuilder
            .from(prefix + displayedText)
            .withAnchor(link.getAnchor())
            .withText(prefix + displayedText)
            .toString();
        errorResult.addReplacement(replacement);
      }

      // Remove the prefix
      replacement = contents.substring(link.getBeginIndex(), link.getEndIndex());
      errorResult.addReplacement(replacement);
    }

    // Remove internal link
    errorResult.addReplacement(prefix + link.getDisplayedText());

    // Missing = in a template argument
    PageElementTemplate template = analysis.isInTemplate(link.getBeginIndex());
    if (template != null) {
      PageElementTemplate.Parameter param = template.getParameterAtIndex(link.getBeginIndex());
      if ((param != null) &&
          StringUtils.isEmpty(param.getName()) &&
          (beginIndex == param.getValueStartIndex())) {
        replacement =
            contents.substring(beginIndex, link.getBeginIndex()) +
            "=" +
            contents.substring(link.getBeginIndex(), endIndex);
        errorResult.addReplacement(replacement);
      }
    }

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
   * @param wiki Wiki.
   * @return True if link and text can be considered identical.
   */
  private boolean areIdentical(String link, String text, EnumWikipedia wiki) {
    link = cleanLink(link, wiki);
    text = cleanLink(text, wiki);
    if (Page.areSameTitle(link, text)) {
      return true;
    }
    if (link.endsWith(")")) {
      int openParenthesis = link.lastIndexOf('(');
      if (openParenthesis > 0) {
        return areIdentical(link.substring(0,  openParenthesis), text, wiki);
      }
    }
    return false;
  }

  /**
   * Tells if it is safe to make a link.
   * 
   * @param link Link.
   * @param text Suggested text for the link.
   * @param wiki Wiki.
   * @return True if it is safe to make a link.
   */
  private boolean isSafeLink(PageElementInternalLink link, String text, EnumWikipedia wiki) {
    if (isSafeLink(link.getFullLink(), text, wiki)) {
      return true;
    }
    if (link.getAnchor() != null) {
      if (isSafeLink(link.getLink(), text, wiki)) {
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
   * @param wiki Wiki.
   * @return True if it is safe to make a link.
   */
  private boolean isSafeLink(String link, String text, EnumWikipedia wiki) {
    if ((link == null) || (text == null)) {
      return false;
    }
    if (Page.areSameTitle(link, text)) {
      return true;
    }
    if (Page.areSameTitle(
        cleanLink(link, wiki),
        cleanLink(text, wiki))) {
      return true;
    }

    // Check if the link can be a disambiguation link (parenthesis or comma)
    int parenthesis = link.indexOf('(');
    if ((parenthesis > 0) && (isSafeLink(link.substring(0, parenthesis), text, wiki))) {
      return true;
    }
    int comma = link.indexOf(',');
    if ((comma > 0) && (isSafeLink(link.substring(0, comma), text, wiki))) {
      return true;
    }

    // Check if the text is a part of the link
    if ((parenthesis < 0) &&
        (comma < 0) &&
        (text.length() > 4)) {
      int spaceIndex = 0;
      while (spaceIndex >= 0) {
        while ((spaceIndex < link.length()) &&
              (link.charAt(spaceIndex) == ' ')) {
          spaceIndex++;
        }
        if (link.length() >= spaceIndex + text.length()) {
          String subLink = link.substring(spaceIndex, spaceIndex + text.length());
          if (Page.areSameTitle(cleanLink(subLink, wiki), cleanLink(text, wiki))) {
            return true;
          }
          spaceIndex = link.indexOf(' ', spaceIndex);
        } else {
          spaceIndex = -1;
        }
      }
    }
    return false;
  }

  /**
   * Clean a link to compare between text and link.
   * 
   * @param link Link to be cleaned.
   * @param wiki Wiki.
   * @return Cleaned link.
   */
  private String cleanLink(@Nonnull String link, EnumWikipedia wiki) {
    return SpecialCharacters
        .replaceAllSpecialCharacters(link.toUpperCase(), wiki)
        .replaceAll("-", " ")
        .replaceAll("’", "'")
        .replaceAll("  ++", " ");
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
    addParameter(new AlgorithmParameter(
        PARAMETER_SEPARATORS,
        GT._T("List of suggestions for separators between the text and the internal link"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "separators",
              GT._T("Suggestion for a separator between the text and the internal link"))
        },
        true));
  }
}
