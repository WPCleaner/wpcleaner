/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a565;

import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementFormatting;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 565 of check wikipedia project.
 * Error 565: nowiki tag between bold/italic.
 */
public class CheckErrorAlgorithm565 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm565() {
    super("nowiki tag between bold/italic");
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

    // Analyze each nowiki tag
    List<PageElementTag> tags = analysis.getCompleteTags(WikiTagType.NOWIKI);
    if ((tags == null) || tags.isEmpty()) {
      return false;
    }
    boolean result = false;
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, tag);
    }
    return result;
  }

  /**
   * Analyze a tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Tag to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag) {

    // Check if there's an error
    if (!tag.isComplete()) {
      return false;
    }
    String contents = analysis.getContents();
    int beginIndex = tag.getCompleteBeginIndex();
    int firstIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, beginIndex - 1, "'");
    int countBefore = beginIndex - firstIndex - 1;
    if (countBefore < 2) {
      return false;
    }
    int endIndex = tag.getCompleteEndIndex();
    int lastIndex = ContentsUtil.moveIndexForwardWhileFound(contents, endIndex, "'");
    int countAfter = lastIndex - endIndex; 
    if (countAfter < 2) {
      return false;
    }
    String inside = StringUtils.EMPTY;
    if (!tag.isFullTag()) {
      inside = contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
      if (!StringUtils.containsOnly(inside, CharacterUtils.WHITESPACE)) {
        return false;
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, firstIndex + 1, lastIndex);
    int minCount = Math.min(countBefore, countAfter);
    boolean countOk = (minCount == 2) || (minCount == 3) || (minCount == 5);
    if (countOk) {
      boolean automatic = canBeAutomatic(countBefore, countAfter, analysis, firstIndex, lastIndex);
      String replacement = 
          contents.substring(firstIndex + 1 + minCount, beginIndex) +
          inside +
          contents.substring(endIndex + minCount, lastIndex);
      errorResult.addReplacement(replacement, automatic);
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * Decide if a replacement can be automatic.
   * 
   * @param countBefore Number of apostrophes before.
   * @param countAfter Number of apostrophes after.
   * @param contents Page contents.
   * @param firstIndex First index.
   * @param lastIndex Last index.
   * @return True if the replacement can be automatic.
   */
  private static boolean canBeAutomatic(
      int countBefore, int countAfter,
      PageAnalysis analysis, int firstIndex, int lastIndex) {
    // Check that the number of apostrophes is similar before and after
    if (Math.abs(countAfter - countBefore) >= 2) {
      return false;
    }

    // Check that there are no apostrophes before/after separated by whitespace characters
    String contents = analysis.getContents();
    int tmpIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, firstIndex, " ");
    if ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == '\'')) {
      return false;
    }
    tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, lastIndex, " ");
    if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '\'')) {
      return false;
    }

    // Automatic if number of apostrophes is ok
    if (countAfter == countBefore) {
      return true;
    }
    int minCount = Math.min(countBefore, countAfter);
    if (minCount == 3) {
      return true;
    }

    // For more complex situations, analyze formatting elements around
    List<PageElementFormatting> formatting = PageElementFormatting.listFormattingElements(analysis);
    int mainBegin = formatting.stream()
        .filter(element -> (element.getIndex() >= firstIndex) && (element.getIndex() <= lastIndex))
        .mapToInt(PageElementFormatting::getMainAreaBegin).min().orElse(0);
    int mainEnd = formatting.stream()
        .filter(element -> (element.getIndex() >= firstIndex) && (element.getIndex() <= lastIndex))
        .mapToInt(PageElementFormatting::getMainAreaEnd).max().orElse(contents.length());
    long countElement = formatting.stream().filter(element -> (element.getIndex() >= mainBegin) && (element.getIndex() <= mainEnd)).count();
    if (countElement > 2) {
      return false;
    }

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
    Page page = analysis.getPage();
    if (!page.isArticle() || page.isInTemplateNamespace()) {
      return analysis.getContents();
    }
    if (!analysis.getWPCConfiguration().isEncyclopedicNamespace(page.getNamespace())) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
