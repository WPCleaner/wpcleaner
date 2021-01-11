/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a566;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


/**
 * Algorithm for analyzing error 566 of check wikipedia project.
 * Error 566: abbr tags
 */
public class CheckErrorAlgorithm566 extends CheckErrorAlgorithmTags {

  private static final @Nonnull Set<TagType> tags = Collections.singleton(HtmlTagType.ABBR);

  public CheckErrorAlgorithm566() {
    super("<abbr> tags");

  }

  /**
   * @return Tags to look for.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags#getTags()
   */
  @Override
  protected Set<TagType> getTags() {
    return tags;
  }

  /**
   * Report an error for one tag.
   * 
   * @param analysis Page analysis.
   * @param errors Errors.
   * @param tag Tag.
   */
  @Override
  protected void reportTag(
      @Nonnull PageAnalysis analysis,
      @Nonnull Collection<CheckErrorResult> errors,
      @Nonnull PageElementTag tag) {
    boolean reportComplete = reportCompleteTags();
    String contents = analysis.getContents();
    int beginIndex = reportComplete ? tag.getCompleteBeginIndex() : tag.getBeginIndex();
    while ((beginIndex > 0) &&
        (CharacterUtils.isClassicLetter(contents.charAt(beginIndex - 1)) || Character.isDigit(contents.charAt(beginIndex - 1)))) {
      beginIndex--;
    }
    int endIndex = reportComplete ? tag.getCompleteEndIndex() : tag.getEndIndex();
    while ((endIndex < contents.length()) &&
        (CharacterUtils.isClassicLetter(contents.charAt(endIndex)) || Character.isDigit(contents.charAt(endIndex)))) {
      endIndex++;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    errors.add(errorResult);
  }

}
