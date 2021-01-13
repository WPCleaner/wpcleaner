/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a04x.a049;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;


/**
 * Algorithm for analyzing error 49 of check wikipedia project.
 * Error 49: Headline with HTML
 */
public class CheckErrorAlgorithm049 extends CheckErrorAlgorithmBase {

  /**
   * Tags that can be detected.
   */
  private static final List<TagType> titleTags = new ArrayList<>();
  static {
    titleTags.add(HtmlTagType.H1);
    titleTags.add(HtmlTagType.H2);
    titleTags.add(HtmlTagType.H3);
    titleTags.add(HtmlTagType.H4);
    titleTags.add(HtmlTagType.H5);
    titleTags.add(HtmlTagType.H6);
    titleTags.add(HtmlTagType.H7);
    titleTags.add(HtmlTagType.H8);
    titleTags.add(HtmlTagType.H9);
  }

  public CheckErrorAlgorithm049() {
    super("Headline with HTML");
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

    // Analyzing each possible tag
    boolean result = false;
    for (int level = 0; level < titleTags.size(); level++) {
      TagType tagType = titleTags.get(level);
      List<PageElementTag> tags = analysis.getCompleteTags(tagType);
      if (tags != null) {
        for (PageElementTag tag : tags) {
          // Decide if error should be reported
          boolean shouldReport = true;
          int index = tag.getBeginIndex();
          if (shouldReport &&
              ((analysis.getSurroundingTag(WikiTagType.SOURCE, index) != null) ||
               (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, index) != null))) {
            shouldReport = false;
          }

          // Report error
          if (shouldReport) {
            if (errors == null) {
              return true;
            }
            result = true;
  
            // Find possible replacement
            String replacement = analysis.getContents().substring(
                tag.getValueBeginIndex(), tag.getValueEndIndex());
  
            // Create error
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
            errorResult.addReplacement(TitleBuilder.from(
                level + 1, replacement).toString());
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
