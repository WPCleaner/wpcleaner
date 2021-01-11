/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Base class for errors related to unwanted tags.
 */
public abstract class CheckErrorAlgorithmTags extends CheckErrorAlgorithmBase {

  /** Default tags in which error should be ignored. */
  private final static @Nonnull Set<TagType> IGNORED_TAGS;

  static {
    Set<TagType> tmpSet = new HashSet<>();
    tmpSet.add(WikiTagType.SOURCE);
    tmpSet.add(WikiTagType.SYNTAXHIGHLIGHT);
    IGNORED_TAGS = Collections.unmodifiableSet(tmpSet);
  }

  /**
   * @param name Name of error.
   */
  public CheckErrorAlgorithmTags(String name) {
    super(name);

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
      Collection<CheckErrorResult> errors,
      boolean onlyAutomatic) {

    // Preliminary check
    if (!analysis.getPage().isArticle()) {
      return false;
    }
    List<PageElementTag> allTags = analysis.getTags();
    if ((allTags == null) || (allTags.isEmpty())) {
      return false;
    }
    Set<TagType> tags = getTags();
    if (tags.isEmpty()) {
      return false;
    }

    // Check each tag
    boolean result = false;
    for (PageElementTag tag : allTags) {
      if (tags.contains(tag.getType()) &&
          shouldReport(analysis, tag)) {
        if (errors == null) {
          return true;
        }
        result = true;
        reportTag(analysis, errors, tag);
      }
    }
    return result;
  }

  /**
   * @return Tags to look for.
   */
  abstract protected @Nonnull Set<TagType> getTags();

  /**
   * @return Tags in which error should be ignored.
   */
  protected @Nonnull Set<TagType> getIgnoredTags() {
    return IGNORED_TAGS;
  }

  /**
   * @return True if complete tags should be reported as one tag instead of separate tags.
   */
  protected boolean reportCompleteTags() {
    return true;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag to be analyzed.
   * @return True if tag should be reported.
   */
  protected boolean shouldReport(@Nonnull PageAnalysis analysis, @Nonnull PageElementTag tag) {

    // Check nowiki tags
    if (!WikiTagType.NOWIKI.equals(tag.getType())) {
      if (analysis.getSurroundingTag(
          WikiTagType.NOWIKI,
          tag.getBeginIndex()) != null) {
        return false;
      }
    }

    // Check surrounding tags
    Set<TagType> ignoredTags = getIgnoredTags();
    for (TagType ignoredTag : ignoredTags) {
      if (analysis.getSurroundingTag(ignoredTag, tag.getBeginIndex()) != null) {
        return false;
      }
    }

    // Check complete tags
    if (reportCompleteTags()) {
      if (!tag.isFullTag() && tag.isComplete() && tag.isEndTag()) {
        return false;
      }
    }

    return true;
  }

  /**
   * Report an error for one tag.
   * 
   * @param analysis Page analysis.
   * @param errors Errors.
   * @param tag Tag.
   */
  protected void reportTag(
      @Nonnull PageAnalysis analysis,
      @Nonnull Collection<CheckErrorResult> errors,
      @Nonnull PageElementTag tag) {
    boolean reportComplete = reportCompleteTags();
    int beginIndex = reportComplete ? tag.getCompleteBeginIndex() : tag.getBeginIndex();
    int endIndex = reportComplete ? tag.getCompleteEndIndex() : tag.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    errors.add(errorResult);
  }
}
