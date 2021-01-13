/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a55x.a554;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 554 of check wikipedia project.
 * Error 554: nowiki in gallery tags.
 */
public class CheckErrorAlgorithm554 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm554() {
    super("nowiki in gallery tags");
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
    List<PageElementTag> nowikiTags = analysis.getTags(WikiTagType.NOWIKI);
    if ((nowikiTags == null) || (nowikiTags.isEmpty())) {
      return false;
    }
    List<PageElementTag> galleryTags = analysis.getTags(WikiTagType.GALLERY);
    if ((galleryTags == null) || (galleryTags.isEmpty())) {
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

    // Check if there's a gallery tag around the nowiki tag
    PageElementTag galleryTag = analysis.getSurroundingTag(WikiTagType.GALLERY, nowikiTag.getBeginIndex());
    if (galleryTag == null) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, nowikiTag.getCompleteBeginIndex(), nowikiTag.getCompleteEndIndex());
    String contents = analysis.getContents();
    if (!nowikiTag.isFullTag()) {
      String internalText = contents.substring(
          nowikiTag.getValueBeginIndex(),
          nowikiTag.getValueEndIndex());
      if ((contents.charAt(nowikiTag.getBeginIndex() - 1) == '|') &&
          (internalText.trim().length() == 0)) {
        errorResult.addReplacement("", true);
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
}
