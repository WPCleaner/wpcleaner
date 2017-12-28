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
        }, true),
    new Replacement(
        PageElementTag.TAG_HTML_DIV,
        new String[] {
          PageElementTag.TAG_HTML_BIG,
          PageElementTag.TAG_HTML_CENTER,
          PageElementTag.TAG_HTML_SMALL,
        }, true),
    new Replacement(
        PageElementTag.TAG_HTML_SPAN,
        new String[] {
          PageElementTag.TAG_HTML_BIG,
          PageElementTag.TAG_HTML_SMALL,
        }, true),
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
    String contents = analysis.getContents();
    for (PageElementTag tag : tags) {
      int index = tag.getValueBeginIndex();
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
            result = true;

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

            // Report error
            if (known) {
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
            } else {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, internalTag.getBeginIndex(), internalTag.getEndIndex());
              errors.add(errorResult);
            }
          }
        } else {
          index++;
        }
      }
    }

    return result;
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

    /**
     * @param firstTag Surrounding tag.
     * @param secondTags Tags that should be inside.
     * @param automatic Automatic replacement.
     */
    Replacement(
        String firstTag,
        String[] secondTags,
        boolean automatic) {
      this.firstTag = firstTag;
      this.secondTags = Arrays.asList(secondTags);
      this.automatic = automatic;
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