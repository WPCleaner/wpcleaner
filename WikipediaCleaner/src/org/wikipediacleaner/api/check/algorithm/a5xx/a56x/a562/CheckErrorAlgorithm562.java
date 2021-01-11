/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a562;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.CompleteTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 562 of check wikipedia project.
 * Error 562: Consecutive nowiki tags
 */
public class CheckErrorAlgorithm562 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm562() {
    super("Consecutive nowiki tags");
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

    // Preliminary checks
    List<PageElementTag> nowikiTags = analysis.getCompleteTags(WikiTagType.NOWIKI);
    if ((nowikiTags == null) || (nowikiTags.size() < 2)) {
      return false;
    }

    // Check each nowiki tag
    boolean result = false;
    for (PageElementTag tag : nowikiTags) {
      result |= analyzeTag(analysis, errors, tag);
    }

    return result;
  }

  /**
   * Analyze a nowiki tag to see if it's consecutive with another nowiki tag.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Tag to analyze.
   * @return True if the tag is consecutive with another tag.
   */
  public boolean analyzeTag(PageAnalysis analysis, Collection<CheckErrorResult> errors, PageElementTag tag) {
    if ((analysis == null) || (tag == null)) {
      return false;
    }
    int beginIndex = tag.getCompleteBeginIndex();
    String contents = analysis.getContents();
    while ((beginIndex > 0) &&
           (CharacterUtils.isWhitespace(contents.charAt(beginIndex - 1)))) {
      beginIndex--;
    }
    if (beginIndex <= 0) {
      return false;
    }
    if (contents.charAt(beginIndex - 1) != '>') {
      return false;
    }
    PageElementTag previousTag = analysis.isInTag(beginIndex - 1, WikiTagType.NOWIKI);
    if ((previousTag == null) ||
        !previousTag.isComplete() ||
        (previousTag.getCompleteEndIndex() != beginIndex)) {
      return false;
    }
    if (errors == null) {
      return true;
    }
    beginIndex = previousTag.getCompleteBeginIndex();
    int endIndex = tag.getCompleteEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    if (tag.isFullTag() && previousTag.isFullTag()) {
      errorResult.addReplacement(contents.substring(beginIndex, tag.getCompleteBeginIndex()));
    } else {
      StringBuilder inside = new StringBuilder();
      if (!previousTag.isFullTag()) {
        inside.append(contents.substring(previousTag.getValueBeginIndex(), previousTag.getValueEndIndex()));
      }
      inside.append(contents.substring(previousTag.getCompleteEndIndex(), tag.getCompleteBeginIndex()));
      if (!tag.isFullTag()) {
        inside.append(contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex()));
      }
      errorResult.addReplacement(CompleteTagBuilder.from(WikiTagType.NOWIKI, inside.toString()).toString());
    }
    errors.add(errorResult);
    return true;
  }
}
