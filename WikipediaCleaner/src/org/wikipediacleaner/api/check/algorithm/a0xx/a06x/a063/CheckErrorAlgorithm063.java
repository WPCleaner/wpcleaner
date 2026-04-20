/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a063;

import java.util.Collection;
import java.util.List;
import java.util.Map;

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

  private static final List<TagType> SURROUNDING_REF = List.of(HtmlTagType.SMALL, HtmlTagType.SUB, HtmlTagType.SUP);
  private static final List<TagType> SURROUNDING_SMALL = List.of(WikiTagType.REF, HtmlTagType.SUB, HtmlTagType.SUP);
  private static final Map<TagType, List<TagType>> REMOVE_SURROUNDING = Map.ofEntries(
      Map.entry(WikiTagType.REF, List.of(HtmlTagType.SMALL, HtmlTagType.SUB, HtmlTagType.SUP))
  );

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
    boolean result;
    result = analysis.getTags(HtmlTagType.SMALL).stream()
        .map(tag -> analyzeTag(analysis, errors, tag, SURROUNDING_SMALL, true))
        .reduce(false, Boolean::logicalOr);
    result |= analysis.getTags(WikiTagType.REF).stream()
        .map(tag -> analyzeTag(analysis, errors, tag, SURROUNDING_REF, false))
        .reduce(false, Boolean::logicalOr);
    return result;
  }

  private boolean analyzeTag(
      final PageAnalysis analysis,
      final Collection<CheckErrorResult> errors,
      final PageElementTag tag,
      final List<TagType> surrounding,
      final boolean marginAllowed) {

    // Find closest surrounding tag
    final int index = tag.getBeginIndex();
    PageElementTag surroundingTag = null;
    for (TagType type : surrounding) {
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

    final boolean allComplete = surroundingTag.isComplete() && tag.isComplete();
    final boolean hasMargin = allComplete &&
        surroundingTag.getValueBeginIndex() == tag.getCompleteBeginIndex() &&
        surroundingTag.getValueEndIndex() == tag.getCompleteEndIndex();
    if (hasMargin && !marginAllowed) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    if (!allComplete) {
      CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), tag.getEndIndex());
      errors.add(errorResult);
      return true;
    }

    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        surroundingTag.getCompleteBeginIndex(), surroundingTag.getCompleteEndIndex());
    if (!hasMargin &&
        REMOVE_SURROUNDING.getOrDefault(tag.getType(), List.of()).contains(surroundingTag.getType())) {
      errorResult.addReplacement(
          analysis.getContents().substring(tag.getCompleteBeginIndex(), tag.getCompleteEndIndex()));
    }
    errors.add(errorResult);
    return true;
  }
}
