/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a549;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.utils.string.CharacterUtils;


/**
 * Algorithm for analyzing error 549 of check wikipedia project.
 * Error 549: Split link.
 */
public class CheckErrorAlgorithm549 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm549() {
    super("Split link");
  }

  /** Punctuation characters */
  private static final String PUNCTUATIONS = ".,;:()";

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

    // Check each kind of links
    boolean result = false;
    result |= analyzeInternalLinks(analysis, errors);
    //result |= analyzeExternalLinks(analysis, errors);
    //result |= analyzeInterwikiLinks(analysis, errors);

    return result;
  }

  /**
   * Analyze a page to check if errors are present in internal links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    boolean result = false;
    AtomicInteger linkNum = new AtomicInteger(0);
    while (linkNum.get() < links.size()) {
      result |= analyzeInternalLink(analysis, errors, links, linkNum);
    }
    return result;
  }

  /**
   * Analyze a page to check if errors are present in internal links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      List<PageElementInternalLink> links,
      AtomicInteger linkNum) {

    // Check if next link has the same target
    PageElementInternalLink firstLink = links.get(linkNum.get());
    if (linkNum.addAndGet(1) >= links.size()) {
      return false;
    }
    String fullLink = firstLink.getFullLink();
    if (!fullLink.equals(links.get(linkNum.get()).getFullLink())) {
      return false;
    }
    int endIndex = firstLink.getEndIndex();

    // Check for consecutive links with the same target
    String contents = analysis.getContents();
    List<PageElementInternalLink> sameLinks = new ArrayList<>();
    sameLinks.add(firstLink);
    List<String> textsBetween = new ArrayList<>();
    boolean finished = false;
    while (!finished && (linkNum.get() < links.size())) {

      // Ignore special characters after the link
      StringBuilder textBetween = new StringBuilder();
      int tmpIndex = endIndex;
      while (!finished && (tmpIndex < contents.length())) {
        char tmpChar = contents.charAt(tmpIndex);
        if (tmpChar == '<') {
          PageElementTag tag = analysis.isInTag(tmpIndex);
          if ((tag != null) &&
              (tag.getBeginIndex() == tmpIndex) &&
              (WikiTagType.NOWIKI.equals(tag.getType()))) {
            tmpIndex = tag.getEndIndex();
          } else {
            finished = true;
          }
        } else if (CharacterUtils.isWhitespace(tmpChar) ||
                   (tmpChar == '\'')) {
          textBetween.append(tmpChar);
          tmpIndex++;
        } else {
          finished = true;
        }
      }

      // Check if the next link is just after the current link
      finished = false;
      if ((links.get(linkNum.get()).getBeginIndex() == tmpIndex) &&
          (fullLink.equals(links.get(linkNum.get()).getFullLink()))) {
        textsBetween.add(textBetween.toString());
        PageElementInternalLink lastLink = links.get(linkNum.get());
        sameLinks.add(lastLink);
        endIndex = lastLink.getEndIndex();
        linkNum.addAndGet(1);
      } else {
        finished = true;
      }
    }

    // Check if there's an error
    if (sameLinks.size() <= 1) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Report error
    PageElementInternalLink lastLink = sameLinks.get(sameLinks.size() - 1);
    int beginIndex = firstLink.getBeginIndex();
    endIndex = lastLink.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errors.add(errorResult);

    // Handle special case [[Link]''[[Link]]''
    if (sameLinks.size() == 2 &&
        "''".equals(textsBetween.get(0)) &&
        contents.startsWith("''", endIndex)) {
      boolean automatic = beginIndex == 0 || contents.charAt(beginIndex - 1) != '\'';
      automatic &= endIndex + 2 == contents.length() || contents.charAt(endIndex + 2) != '\'';
      errorResult.addReplacement(
          contents.substring(firstLink.getEndIndex(), endIndex),
          automatic);
    }
    
    // Construct possible replacement
    StringBuilder buffer = new StringBuilder();
    String previousText = "";
    boolean automatic = true;
    for (int tmpLinkNum = 0; tmpLinkNum < sameLinks.size(); tmpLinkNum++) {
      PageElementInternalLink link = sameLinks.get(tmpLinkNum);
      String text = link.getDisplayedTextNotTrimmed();
      boolean nowiki = false;
      boolean punctuation = false;
      boolean otherChar = false;
      if (link.getTextOffset() > 0) {
        int charNum = 0;
        while (charNum < text.length()) {
          int currentIndex = link.getBeginIndex() + link.getTextOffset() + charNum;
          char currentChar = contents.charAt(currentIndex);
          if (currentChar == '<') {
            PageElementTag tag = analysis.isInTag(currentIndex);
            if ((tag != null) &&
                (tag.getBeginIndex() == currentIndex) &&
                (WikiTagType.NOWIKI.equals(tag.getType()))) {
              nowiki = true;
              charNum += tag.getEndIndex() - tag.getBeginIndex() - 1;
            } else {
              otherChar = true;
            }
          } else if (PUNCTUATIONS.indexOf(currentChar) >= 0) {
            punctuation = true;
          } else if (!CharacterUtils.isWhitespace(currentChar)) {
            otherChar = true;
          }
          charNum++;
        }
      } else {
        otherChar = true;
      }
      boolean textEquals = text.equals(previousText);
      boolean textEqualsCase = text.equalsIgnoreCase(previousText);
      previousText = text;
      if (textEqualsCase) {
        automatic = false;
      }
      if (tmpLinkNum > 0) {
        String between = textsBetween.get(tmpLinkNum - 1);
        if (!between.isEmpty()) {
          buffer.append(between);
          if (between.startsWith("'") || between.endsWith("'")) {
            automatic = false;
          }
        }
      }
      if (!textEquals && (!nowiki || otherChar || punctuation)) {
        if (text.startsWith("'") &&
            buffer.toString().endsWith("'")) {
          automatic = false;
        }
        buffer.append(text);
        if (nowiki) {
          automatic = false;
        }
        if (punctuation && !otherChar) {
          automatic = false;
        } 
      }
    }
    if (buffer.length() == 0) {
      automatic = false;
    }

    // Suggestion to group the links
    errorResult.addReplacement(
        InternalLinkBuilder.from(fullLink).withText(buffer.toString()).toString(),
        automatic);

    // Suggestion to keep only the text for the last link
    boolean lastPunctuationOnly = false;
    String lastText = lastLink.getDisplayedText();
    if (lastText != null) {
      lastPunctuationOnly = true;
      for (int charIndex = 0; charIndex < lastText.length(); charIndex++) {
        if (PUNCTUATIONS.indexOf(lastText.charAt(charIndex)) < 0) {
          lastPunctuationOnly = false;
        }
      }
    }
    errorResult.addReplacement(
        contents.substring(beginIndex, lastLink.getBeginIndex()) + lastText,
        !automatic && lastPunctuationOnly && !fullLink.endsWith(lastText));

    // Suggestion to keep only the text for the first link
    errorResult.addReplacement(
        firstLink.getDisplayedText() + contents.substring(firstLink.getEndIndex(), endIndex),
        false);

    // Suggestion to remove the last link entirely
    errorResult.addReplacement(
        contents.substring(beginIndex, lastLink.getBeginIndex()),
        false);

    // Suggestion to remove the first link entirely
    errorResult.addReplacement(
        contents.substring(firstLink.getEndIndex(), endIndex),
        false);

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
}
