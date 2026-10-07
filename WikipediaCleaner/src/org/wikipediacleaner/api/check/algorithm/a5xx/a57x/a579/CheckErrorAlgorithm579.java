/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a579;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 579 of check wikipedia project.
 * <br>
 * Error 579: Tag simplification.
 */
@SuppressWarnings("unused")
public class CheckErrorAlgorithm579 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm579() {
    super("Tag simplification");
  }

  private static final List<TagType> TAG_TYPES = List.of(WikiTagType.REF);
  private static final Set<TagType> IGNORE_WHITESPACE = Set.of(WikiTagType.REF);
  private static final Set<TagType> DELETE_WITHOUT_ATTRIBUTES = Set.of(WikiTagType.REF);

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

    // Check each tag type
    boolean result = false;
    for (TagType tagType : TAG_TYPES) {
      result |= analyzeTagType(analysis, errors, tagType);
    }

    return result;
  }

  /**
   * Analyze a tag type to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagType Tag type.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTagType(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      TagType tagType) {

    // Check each tag
    List<PageElementTag> tags = analysis.getCompleteTags(tagType);
    if (tags == null) {
      return false;
    }
    boolean result = false;
    boolean ignoreWhitespace = IGNORE_WHITESPACE.contains(tagType);
    boolean deleteWithoutAttributes = DELETE_WITHOUT_ATTRIBUTES.contains(tagType);
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, tag, ignoreWhitespace, deleteWithoutAttributes);
    }
    return result;
  }

  /**
   * Analyze a tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Tag.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag,
      boolean ignoreWhitespace,
      boolean deleteWithoutAttributes) {

    // Check if tag has problems
    if (!tag.isComplete() || tag.isFullTag()) {
      return false;
    }
    String contents = analysis.getContents();
    int tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, tag.getValueBeginIndex(), " ");
    if (tag.getValueEndIndex() > tmpIndex) {
      return false;
    }
    if (analysis.getSurroundingTag(WikiTagType.NOWIKI, tmpIndex) != null) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }

    int beginIndex = tag.getCompleteBeginIndex();
    int endIndex = tag.getCompleteEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    boolean whitespaceOK = (tag.getValueEndIndex() == tag.getValueBeginIndex()) || ignoreWhitespace;
    if (deleteWithoutAttributes && tag.getParametersCount() == 0) {
      errorResult.addReplacement("", whitespaceOK);
    } else {
      String replacement = contents.substring(tag.getBeginIndex(), tag.getEndIndex() - 1) + " />";
      errorResult.addReplacement(
          replacement,
          whitespaceOK);
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
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
