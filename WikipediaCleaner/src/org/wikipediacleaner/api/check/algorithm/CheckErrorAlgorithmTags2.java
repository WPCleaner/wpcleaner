/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Base class for errors related to unwanted tags.
 */
public abstract class CheckErrorAlgorithmTags2 extends CheckErrorAlgorithmBase {

  /**
   * Default tags in which error should be ignored.
   */
  private final static @Nonnull Set<String> IGNORED_TAGS = new HashSet<>();

  static {
    IGNORED_TAGS.add(PageElementTag.TAG_WIKI_SOURCE);
    IGNORED_TAGS.add(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT);
  }

  /**
   * @param name Name of error.
   */
  public CheckErrorAlgorithmTags2(String name) {
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
    Set<String> tags = getTags();
    if (tags.isEmpty()) {
      return false;
    }

    // Check each tag
    boolean result = false;
    for (PageElementTag tag : allTags) {
      if (tags.contains(tag.getNormalizedName()) &&
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
  abstract protected @Nonnull Set<String> getTags();

  /**
   * @return Tags in which error should be ignored.
   */
  protected @Nonnull Set<String> getIgnoredTags() {
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
    if (!PageElementTag.TAG_WIKI_NOWIKI.equalsIgnoreCase(tag.getNormalizedName())) {
      if (analysis.getSurroundingTag(
          PageElementTag.TAG_WIKI_NOWIKI,
          tag.getBeginIndex()) != null) {
        return false;
      }
    }

    // Check surrounding tags
    Set<String> ignoredTags = getIgnoredTags();
    for (String ignoredTag : ignoredTags) {
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
  private void reportTag(
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
