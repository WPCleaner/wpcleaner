/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


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
  private final static String[] TAGS = {
    PageElementTag.TAG_HTML_LI,
    PageElementTag.TAG_HTML_OL,
    PageElementTag.TAG_HTML_UL,
  };

  /**
   * @return Tags to look for.
   */
  @Override
  protected String[] getTags() {
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
    if (PageElementTag.TAG_HTML_LI.equalsIgnoreCase(tag.getNormalizedName())) {
      if (tag.getParameter("value") != null) {
        return false;
      }
    }
    return true;
  }
}
