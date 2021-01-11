/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a01x.a012;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


/**
 * Algorithm for analyzing error 12 of check wikipedia project.
 * Error 12: HTML List elements
 */
public class CheckErrorAlgorithm012 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm012() {
    super("HTML List elements");
  }

  /**
   * Tags to look for.
   */
  private final static Set<TagType> TAGS;

  static {
    Set<TagType> tmpSet = new HashSet<>();
    tmpSet.add(HtmlTagType.LI);
    tmpSet.add(HtmlTagType.OL);
    tmpSet.add(HtmlTagType.UL);
    TAGS = Collections.unmodifiableSet(tmpSet);
  }

  /**
   * @return Tags to look for.
   */
  @Override
  protected Set<TagType> getTags() {
    return TAGS;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag to be analyzed.
   * @return True if tag should be reported.
   */
  @Override
  protected boolean shouldReport(PageAnalysis analysis, PageElementTag tag) {
    if (!super.shouldReport(analysis, tag)) {
      return false;
    }
    if (HtmlTagType.LI.equals(tag.getType())) {
      if (tag.getParameter("value") != null) {
        return false;
      }
    }
    return true;
  }

  /**
   * @return True if complete tags should be reported as one tag instead of separate tags.
   */
  @Override
  protected boolean reportCompleteTags() {
    return false;
  }
}
