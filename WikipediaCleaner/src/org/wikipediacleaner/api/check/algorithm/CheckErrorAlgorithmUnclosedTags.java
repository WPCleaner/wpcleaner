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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.ContentsTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.ContentsTagFormat;


/**
 * Algorithm for analyzing errors based on unclosed tags.
 */
public abstract class CheckErrorAlgorithmUnclosedTags extends CheckErrorAlgorithmBase {

  /**
   * @param name Name of the error.
   */
  public CheckErrorAlgorithmUnclosedTags(String name) {
    super(name);
  }

  /**
   * @return List of tags managed by this error.
   */
  protected abstract List<String> getTags();

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

    // Analyze each tag
    boolean result = false;
    for (String tagName : getTags()) {
      List<PageElementTag> tags = analysis.getTags(tagName);
      for (PageElementTag tag : tags) {
        int beginIndex = tag.getBeginIndex();
        int endIndex = tag.getEndIndex();
        if (!tag.isComplete() || (tag.isFullTag() && reportFullTags())) {

          // Check error
          boolean shouldReport = true;
          if (!PageElementTag.TAG_WIKI_NOWIKI.equals(tagName)) {
            if (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, beginIndex) != null) {
              shouldReport = false;
            }
            if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, beginIndex) != null) ||
                (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, beginIndex) != null)) {
              shouldReport = false;
            }
          }

          // Unclosed tag
          if (shouldReport) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, endIndex);
            errorResult.addReplacement("");
            errors.add(errorResult);
          }
        } else if (tag.isEndTag()) {

          // Closing tag with white space (detected by CW)
          String contents = analysis.getContents();
          if ((contents.charAt(endIndex - 1) == '>') &&
              (contents.charAt(endIndex - 2) == ' ')) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, endIndex, ErrorLevel.WARNING);
            errorResult.addReplacement(
                ContentsTagBuilder.from(tagName, ContentsTagFormat.CLOSE).toString(),
                tag.getParametersCount() == 0);
            errors.add(errorResult);
          }
        } else {

          // Opening tag with white space (detected by CW)
          String contents = analysis.getContents();
          if ((contents.charAt(beginIndex) == '<') &&
              (contents.charAt(beginIndex + 1) == ' ')) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, endIndex, ErrorLevel.WARNING);
            errorResult.addReplacement(
                ContentsTagBuilder.from(tagName, ContentsTagFormat.OPEN).toString(),
                tag.getParametersCount() == 0);
            errors.add(errorResult);
          }
        }
      }
    }
    return result;
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

  /**
   * @return True if full tags should be reported.
   */
  protected boolean reportFullTags() {
    return false;
  }
}
