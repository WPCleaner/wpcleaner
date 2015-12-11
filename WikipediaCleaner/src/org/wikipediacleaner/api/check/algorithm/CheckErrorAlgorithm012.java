/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;


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
}
