/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a579;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class CheckErrorAlgorithm579 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm579.class);

  public CheckErrorAlgorithm579() {
    super("Tag simplification");
  }

  private static Map<TagType, Boolean> tagTypes = new HashMap<>();
  
  static {
    tagTypes.put(WikiTagType.REF, Boolean.TRUE);
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

    // Check each tag type
    boolean result = false;
    for (Entry<TagType, Boolean> entry : tagTypes.entrySet()) {
      result |= analyzeTagType(analysis, errors, entry.getKey(), entry.getValue());
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
      TagType tagType,
      Boolean ignoreWhitespace) {

    // Check each tag
    List<PageElementTag> tags = analysis.getCompleteTags(tagType);
    if (tags == null) {
      return false;
    }
    boolean result = false;
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, tag, ignoreWhitespace);
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
      Boolean ignoreWhitespace) {

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
    String replacement = contents.substring(tag.getBeginIndex(), tag.getEndIndex() - 1) + " />";
    errorResult.addReplacement(
        replacement,
        (tag.getValueEndIndex() == tag.getValueBeginIndex()) || Boolean.TRUE.equals(ignoreWhitespace));
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
