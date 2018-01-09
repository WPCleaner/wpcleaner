/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 539 of check wikipedia project.
 * Error 539: Misnested tags (see [[Special:LintErrors/misnested-tag]])
 */
public class CheckErrorAlgorithm539 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm539() {
    super("Misnested tags");
  }

  /** Possible replacements */
  private final static Replacement[] replacements = {
    new Replacement(
        PageElementTag.TAG_HTML_CENTER,
        new String[] {
          PageElementTag.TAG_HTML_BIG,
          PageElementTag.TAG_HTML_SMALL,
        }, true, false),
    new Replacement(
        PageElementTag.TAG_HTML_DIV,
        new String[] {
          PageElementTag.TAG_HTML_BIG,
          PageElementTag.TAG_HTML_CENTER,
          PageElementTag.TAG_HTML_SMALL,
        }, true, false),
    new Replacement(
        PageElementTag.TAG_HTML_SMALL,
        new String[] {
          PageElementTag.TAG_HTML_CENTER,
        }, true, true),
    new Replacement(
        PageElementTag.TAG_HTML_SPAN,
        new String[] {
          PageElementTag.TAG_HTML_BIG,
          PageElementTag.TAG_HTML_SMALL,
        }, true, false),
  };

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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each type of tag
    boolean result = false;
    for (Replacement replacement : replacements) {
      result |= analyzeTags(analysis, errors, replacement);
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param replacement Possible replacement.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTags(
    PageAnalysis analysis,
    Collection<CheckErrorResult> errors,
    Replacement replacement) {

    // Analyze each tag
    boolean result = false;
    List<PageElementTag> tags = analysis.getCompleteTags(replacement.firstTag);
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, replacement, tag);
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param replacement Possible replacement.
   * @param tag Tag currently being analyzed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      Replacement replacement,
      PageElementTag tag) {

    // Ignore whitespace characters at the beginning
    String contents = analysis.getContents();
    int index = tag.getValueBeginIndex();
    while ((index < tag.getValueEndIndex()) &&
           (" \n".indexOf(contents.charAt(index)) >= 0)) {
      index++;
    }

    // Select area on the reverse option
    int maxValue = tag.getValueEndIndex();
    if (replacement.shouldReverse) {
      maxValue = Math.min(maxValue, index + 1);
    }

    // Analyze area
    while (index < tag.getValueEndIndex()) {

      // Look for an other tag
      PageElementTag internalTag = null;
      if (contents.charAt(index) == '<') {
        internalTag = analysis.isInTag(index);
      }

      // Analyze tag
      if (internalTag != null) {
        index = internalTag.getEndIndex();
        if (internalTag.isComplete() &&
            (internalTag.getCompleteEndIndex() > tag.getCompleteEndIndex())) {
          if (errors == null) {
            return true;
          }

          // Analyze if a replacement can be suggested
          boolean known = replacement.secondTags.contains(internalTag.getNormalizedName());
          int tmpIndex = tag.getCompleteEndIndex();
          while ((tmpIndex < internalTag.getValueEndIndex()) &&
                 (" \n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
          }
          if (tmpIndex < internalTag.getValueEndIndex()) {
            known = false;
          }
          if (tmpIndex >= maxValue) {
            known = false;
          }

          // Report error
          if (known) {
            if (replacement.shouldReverse) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
              String text =
                  contents.substring(internalTag.getCompleteBeginIndex(), internalTag.getValueBeginIndex()) +
                  contents.substring(tag.getCompleteBeginIndex(), internalTag.getCompleteBeginIndex()) +
                  contents.substring(internalTag.getValueBeginIndex(), tag.getCompleteEndIndex());
              String desc =
                  contents.substring(internalTag.getCompleteBeginIndex(), internalTag.getValueBeginIndex()) +
                  contents.substring(tag.getCompleteBeginIndex(), tag.getValueBeginIndex()) +
                  "..." +
                  contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex());
              errorResult.addReplacement(text, desc, replacement.automatic);
              errors.add(errorResult);
            } else {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, internalTag.getCompleteBeginIndex(), internalTag.getCompleteEndIndex());
              String text =
                  contents.substring(internalTag.getCompleteBeginIndex(), tag.getValueEndIndex()) +
                  contents.substring(tag.getCompleteEndIndex(), internalTag.getCompleteEndIndex()) +
                  contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex());
              String desc =
                  contents.substring(internalTag.getCompleteBeginIndex(), internalTag.getValueBeginIndex()) +
                  "..." +
                  contents.substring(internalTag.getValueEndIndex(), internalTag.getCompleteEndIndex()) +
                  contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex());
              errorResult.addReplacement(text,  desc, replacement.automatic);
              errors.add(errorResult);
            }
          } else {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, internalTag.getBeginIndex(), internalTag.getEndIndex());
            errors.add(errorResult);
          }
          return true;
        }
      } else {
        index++;
      }
    }

    return false;
  }

  /**
   * Bean for holding configuration for replacements.
   */
  private static class Replacement {

    /** First tag: surrounding */
    final String firstTag;

    /** Second tag: should be inside */
    final List<String> secondTags;

    /** True if replacement can be automatic */
    final boolean automatic;

    /** True if tags order should be reversed */
    final boolean shouldReverse;

    /**
     * @param firstTag Surrounding tag.
     * @param secondTags Tags that should be inside.
     * @param automatic Automatic replacement.
     * @param shouldReverse True if tag order should be reversed.
     */
    Replacement(
        String firstTag,
        String[] secondTags,
        boolean automatic,
        boolean shouldReverse) {
      this.firstTag = firstTag;
      this.secondTags = Arrays.asList(secondTags);
      this.automatic = automatic;
      this.shouldReverse = shouldReverse;
    }
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}