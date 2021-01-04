/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Base class for errors related to unwanted tags.
 * TODO: Replace with CheckErrorAlgorithmTags2.
 */
public abstract class CheckErrorAlgorithmTags extends CheckErrorAlgorithmBase {

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
  public boolean analyze(PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    boolean result = false;
    String[] tags = getTags();
    if (tags != null) {
      for (String tag : tags) {
        if (!result || (errors != null)) {
          result |= addTags(analysis, errors, tag);
        }
      }
    }
    return result;
  }

  /**
   * @return Tags to look for.
   */
  abstract protected String[] getTags();

  /**
   * Default tags in which error should be ignored.
   */
  private final static String[] IGNORED_TAGS = {
    PageElementTag.TAG_WIKI_SOURCE,
    PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT,
  };

  /**
   * @return Tags in which error should be ignored.
   */
  protected String[] getIgnoredTags() {
    return IGNORED_TAGS;
  }

  /**
   * Find tags.
   * 
   * @param analysis Page analysis.
   * @param errors Errors.
   * @param tagName Tag name.
   * @return Flag indicating if a tag has been found.
   */
  private boolean addTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, String tagName) {
    boolean result = false;
    Collection<PageElementTag> tags = analysis.getTags(tagName);
    if (tags != null) {
      for (PageElementTag tag : tags) {
        if (shouldReport(analysis, tag)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              tag.getBeginIndex(), tag.getEndIndex());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag to be analyzed.
   * @return True if tag should be reported.
   */
  protected boolean shouldReport(PageAnalysis analysis, PageElementTag tag) {
    if (tag == null) {
      return false;
    }
    if (!PageElementTag.TAG_WIKI_NOWIKI.equalsIgnoreCase(tag.getNormalizedName())) {
      if (analysis.getSurroundingTag(
          PageElementTag.TAG_WIKI_NOWIKI,
          tag.getBeginIndex()) != null) {
        return false;
      }
    }
    String[] ignoredTags = getIgnoredTags();
    if (ignoredTags != null) {
      for (String ignoredTag : ignoredTags) {
        if (analysis.getSurroundingTag(ignoredTag, tag.getBeginIndex()) != null) {
          return false;
        }
      }
    }
    return true;
  }
}
