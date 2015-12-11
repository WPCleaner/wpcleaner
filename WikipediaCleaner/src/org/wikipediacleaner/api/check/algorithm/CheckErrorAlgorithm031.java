/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 31 of check wikipedia project.
 * Error 31: HTML table element
 */
public class CheckErrorAlgorithm031 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm031() {
    super("HTML table element");
  }

  /**
   * Tags to look for.
   */
  private final static String[] TAGS = {
    PageElementTag.TAG_HTML_TABLE,
    PageElementTag.TAG_HTML_TD,
    PageElementTag.TAG_HTML_TH,
    PageElementTag.TAG_HTML_TR,
  };

  /**
   * @return Tags to look for.
   */
  @Override
  protected String[] getTags() {
    return TAGS;
  }
}
