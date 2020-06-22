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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 555 of check wikipedia project.
 * Error 554: nowiki in text.
 */
public class CheckErrorAlgorithm555 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm555() {
    super("nowiki in text");
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

    // Only work on complete tags
    if ((nowikiTag == null) || !nowikiTag.isComplete()) {
      return false;
    }
    if (!nowikiTag.isFullTag() && nowikiTag.isEndTag()) {
      return false;
    }

    // Check character before the tag
    String contents = analysis.getContents();
    int beginIndex = nowikiTag.getCompleteBeginIndex();
    if (beginIndex > 0) {
      if (!isAcceptable(contents.charAt(beginIndex - 1))) {
        return false;
      }
    }

    // Check character after the tag
    int endIndex = nowikiTag.getCompleteEndIndex();
    if (endIndex < contents.length()) {
      if (!isAcceptable(contents.charAt(endIndex))) {
        return false;
      }
    }

    // Check content inside the tag
    if (!nowikiTag.isFullTag()) {
      for (int index = nowikiTag.getValueBeginIndex(); index < nowikiTag.getValueEndIndex(); index++) {
        if (!isAcceptable(contents.charAt(index))) {
          return false;
        }
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, nowikiTag.getCompleteBeginIndex(), nowikiTag.getCompleteEndIndex());
    if (nowikiTag.isFullTag()) {
      errorResult.addReplacement("");
    } else {
      errorResult.addReplacement(contents.substring(
          nowikiTag.getValueBeginIndex(),
          nowikiTag.getValueEndIndex()));
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * @param character Character to be tested.
   * @return True if the character is acceptable for this error.
   */
  private boolean isAcceptable(char character) {
    return
        Character.isAlphabetic(character) ||
        Character.isDigit(character) ||
        Character.isWhitespace(character);
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
