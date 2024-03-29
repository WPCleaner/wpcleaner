/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a55x.a550;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.string.CharacterUtils;


/**
 * Algorithm for analyzing error 550 of check wikipedia project.
 * Error 550: Link without text.
 */
public class CheckErrorAlgorithm550 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm550() {
    super("Link without text");
  }

  private final static List<String> WITH_PUNCTUATION = Stream
      .of(
          " ",
          "&nbsp;",
          "''&nbsp;''")
      .collect(Collectors.toList());

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

    // Check each link
    boolean result = false;
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    PageElementInternalLink previousLink = null;
    for (PageElementInternalLink link : links) {
      boolean linkResult = analyzeLink(analysis, errors, link, previousLink);
      if (linkResult) {
        previousLink = link;
      }
      result |= linkResult;
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
  private boolean analyzeLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link,
      PageElementInternalLink previousLink) {

    // Analyze link to see if it's empty
    String target = link.getFullLink();
    if ((target == null) || (target.isEmpty())) {
      return false;
    }
    String text = link.getDisplayedTextNotTrimmed();
    if ((text == null) || text.isEmpty()) {
      return false;
    }
    boolean hasSpace = false;
    boolean hasNBSP = false;
    boolean automatic = true;
    int index = 0;
    while (index < text.length()) {
      char currentChar = text.charAt(index);
      if (currentChar == '<') {
        PageElementTag tag = PageElementTag.analyzeBlock(text, index);
        if ((tag != null) &&
            WikiTagType.NOWIKI.equals(tag.getType())) {
          index = tag.getEndIndex();
        } else {
          return false;
        }
      } else if (CharacterUtils.isWhitespace(currentChar)) {
        index++;
        automatic = false;
        hasSpace = true;
      } else if (currentChar == '&') {
        if (text.startsWith("&nbsp;", index)) {
          hasNBSP = true;
          index += 6;
        } else {
          return false;
        }
      } else if (currentChar == '\'') {
        int quotesCount = ContentsUtil.moveIndexForwardWhileFound(text, index, "'") - index;
        if ((quotesCount == 2) || (quotesCount == 3) || (quotesCount == 5)) {
          index += quotesCount;
        } else {
          return false;
        }
      } else {
        return false;
      }
    }

    // Ignore some special situations
    if (ignoreLinks.contains(link.getLink())) {
      return false;
    }
    PageElementFunction function = analysis.isInFunction(link.getBeginIndex());
    if ((function != null) &&
        Objects.equals(function.getFunctionName(), "#tag") &&
        (function.getParameterCount() > 0) &&
        Objects.equals(function.getParameterValue(0), "timeline")) {
      return false;
    }

    // Extend error area
    if (errors == null) {
      return true;
    }
    String contents = analysis.getContents();
    int beginIndex = link.getBeginIndex();
    int endIndex = link.getEndIndex();
    int quotesAfter = ContentsUtil.moveIndexForwardWhileFound(contents, endIndex, "'") - endIndex;
    int quotesBefore = beginIndex - ContentsUtil.moveIndexBackwardWhileFound(contents, beginIndex - 1, "'") - 1;
    if ((quotesAfter != quotesBefore) ||
        ((quotesAfter != 2) && (quotesAfter != 3) && (quotesAfter != 5))) {
      quotesAfter = 0;
      quotesBefore = 0;
    }
    beginIndex -= quotesBefore;
    endIndex += quotesAfter;

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);

    // Replace by whitespace if close to some punctuation
    if (WITH_PUNCTUATION.contains(text)) {
      boolean withPunctuation = false;
      if ((beginIndex > 0) &&
          ("«—".indexOf(contents.charAt(beginIndex - 1)) >= 0)) {
        withPunctuation = true;
      }
      if ((endIndex < contents.length()) &&
          ("»:;%—".indexOf(contents.charAt(endIndex)) >= 0)) {
        withPunctuation = true;
      }
      if (withPunctuation) {
        errorResult.addReplacement(" ", true);
        automatic = false;
      }
    }

    // Check with previous link
    if ((previousLink != null) && (previousLink.getEndIndex() == beginIndex)) {
      // TODO
    }

    automatic = false; //TODO

    // Report error
    errorResult.addReplacement("", automatic);
    if (hasSpace || hasNBSP) {
      boolean otherSpace = false;
      if ((beginIndex > 0) &&
          CharacterUtils.isWhitespace(contents.charAt(beginIndex - 1))) {
        otherSpace = true;
      }
      if ((endIndex < contents.length()) &&
          CharacterUtils.isWhitespace(contents.charAt(endIndex))) {
        otherSpace = true;
      }
      if (!otherSpace) {
        errorResult.addReplacement(" ");
      }
    }
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

  /** List of links to be ignored */
  private static final String PARAMETER_IGNORE_LINKS = "ignore_links";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_LINKS, true, true, true);
    ignoreLinks.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        ignoreLinks.addAll(tmpList);
      }
    }
  }

  /** Links to ignore */
  private final Set<String> ignoreLinks = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_LINKS,
        GT._T("Links to ignore"),
        new AlgorithmParameterElement(
            "article name",
            GT._T("Link to ignore")),
        true));
  }
}
