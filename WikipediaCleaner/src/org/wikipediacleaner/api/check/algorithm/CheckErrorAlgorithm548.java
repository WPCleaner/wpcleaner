/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;


/**
 * Algorithm for analyzing error 548 of check wikipedia project.
 * Error 548: Punctuation in link.
 */
public class CheckErrorAlgorithm548 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm548() {
    super("Punctuation in link");
  }

  private static final String PUNCTUATIONS = ",;"; // Avoid ":" and "."

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

    // Only in main name space
    if ((analysis.getPage().getNamespace() == null) ||
        (analysis.getPage().getNamespace().intValue() != Namespace.MAIN)) {
      return false;
    }

    // Check each kind of links
    boolean result = false;
    result |= analyzeInternalLinks(analysis, errors);
    result |= analyzeExternalLinks(analysis, errors);
    result |= analyzeInterwikiLinks(analysis, errors);

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
    for (PageElementInternalLink link : links) {
      if (link.getText() != null) {
        int beginText = link.getBeginIndex() + link.getTextOffset();
        int endText = link.getEndIndex() - 2;
        result |= analyzeLink(analysis, errors, link, beginText, endText);
      }
    }
    return result;
  }

  /**
   * Analyze a page to check if errors are present in external links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeExternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return false;
    }
    boolean result = false;
    for (PageElementExternalLink link : links) {
      if (link.getText() != null) {
        int beginText = link.getBeginIndex() + link.getTextOffset();
        int endText = link.getEndIndex() - 1;
        result |= analyzeLink(analysis, errors, link, beginText, endText);
      }
    }
    return result;
  }

  /**
   * Analyze a page to check if errors are present in interwiki links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInterwikiLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementInterwikiLink> links = analysis.getInterwikiLinks();
    if (links == null) {
      return false;
    }
    boolean result = false;
    for (PageElementInterwikiLink link : links) {
      if (link.getText() != null) {
        int beginText = link.getBeginIndex() + link.getTextOffset();
        int endText = link.getEndIndex() - 2;
        result |= analyzeLink(analysis, errors, link, beginText, endText);
      }
    }
    return result;
  }

  /**
   * Analyze a link to check if a punctuation is inside it.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Link to analyze.
   * @param beginText Beginning of the text to analyze.
   * @param endText End of the text to analyze.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElement link, int beginText, int endText) {

    // Analyze text
    String contents = analysis.getContents();
    int punctuationLength = findPunctuationAtEnd(contents.substring(beginText, endText));
    if (punctuationLength == 0) {
      return false;
    }
    // Note: special trick to avoid modifying incorrectly parsed links (a link should finish by a ])
    if ((endText + 1 >= contents.length()) || (contents.charAt(endText) != ']')) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, link.getBeginIndex(), link.getEndIndex());
    String punctuationText = contents.substring(endText - punctuationLength, endText);
    if (punctuationLength < endText - beginText) {
      String replacement =
          contents.substring(link.getBeginIndex(), endText - punctuationLength) +
          contents.substring(endText, link.getEndIndex()) +
          punctuationText;
      boolean automatic = true;
      if (punctuationText.indexOf('.') >= 0) {
        automatic = false;
      }
      errorResult.addReplacement(replacement, automatic);
    } else {
      errorResult.addReplacement(punctuationText, false);
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * Find the length of text including a punctuation at the end.
   * 
   * @param text Text to analyze.
   * @return Number of characters that could be extracted.
   */
  private int findPunctuationAtEnd(String text) {
    if (text == null) {
      return 0;
    }

    // Trim final white space characters
    int endIndex = text.length() - 1;
    while ((endIndex >= 0) && CharacterUtils.isWhitespace(text.charAt(endIndex))) {
      endIndex--;
    }
    if (endIndex < 0) {
      return 0;
    }

    // Check if it ends with a punctuation sign
    char lastChar = text.charAt(endIndex);
    if (PUNCTUATIONS.indexOf(lastChar) < 0) {
      return 0;
    }
    endIndex--;

    // Handle HTML characters like &nbsp;
    if (lastChar == ';') {
      int tmpIndex = endIndex;
      while ((tmpIndex > 0) && Character.isLetterOrDigit(text.charAt(tmpIndex))) {
        tmpIndex--;
      }
      if ((tmpIndex >= 0) && ("&#".indexOf(text.charAt(tmpIndex)) >= 0)) {
        return 0;
      }
    }

    // Remove extra white space characters
    while ((endIndex > 0) && CharacterUtils.isWhitespace(text.charAt(endIndex))) {
      endIndex--;
    }

    return text.length() - endIndex - 1;
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
