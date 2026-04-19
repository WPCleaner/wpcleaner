/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a063;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 63 of check wikipedia project.
 * Error 63: HTML text style element &lt;small&gt; in ref, sub or sup
 */
@SuppressWarnings("unused")
public class CheckErrorAlgorithm063 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm063() {
    super("HTML text style element <small>, <sub> or <sup> in ref, small, sub or sup");
  }

  private static final List<TagType> ANALYZED_TAGS = List.of(HtmlTagType.SMALL, HtmlTagType.SUB, HtmlTagType.SUP);
  private static final List<TagType> SURROUNDING_TAGS = List.of(WikiTagType.REF, HtmlTagType.SMALL, HtmlTagType.SUB, HtmlTagType.SUP);

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
      final PageAnalysis analysis,
      final Collection<CheckErrorResult> errors, final boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyze each tag
    boolean result = false;
    for (TagType analyzed : ANALYZED_TAGS) {
      List<PageElementTag> tags = analysis.getTags(analyzed);
      for (PageElementTag tag : tags) {
        result |= analyzeTag(analysis, errors, tag);
      }
    }

    return result;
  }

  private boolean analyzeTag(
      final PageAnalysis analysis,
      final Collection<CheckErrorResult> errors,
      final PageElementTag tag) {

    // Find closest surrounding tag
    final int index = tag.getBeginIndex();
    PageElementTag surroundingTag = null;
    for (TagType type : SURROUNDING_TAGS) {
      PageElementTag currentTag = analysis.getSurroundingTag(type, index);
      if (currentTag != null) {
        if (surroundingTag == null || surroundingTag.getBeginIndex() < currentTag.getBeginIndex()) {
          surroundingTag = currentTag;
        }
      }
    }
    if (surroundingTag == null) {
      return false;
    }

    // Ignore small inside small (detected by #055)
    if (tag.getType() == HtmlTagType.SMALL && surroundingTag.getType() == HtmlTagType.SMALL) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        tag.getBeginIndex(), tag.getEndIndex());
    errors.add(errorResult);
    return true;
  }
}
