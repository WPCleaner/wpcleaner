/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 40 of check wikipedia project.
 * Error 40: HTML text style element &lt;font&gt;
 */
public class CheckErrorAlgorithm040 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm040() {
    super("HTML text style element <font>");
  }

  /**
   * Tags to look for.
   */
  private final static String[] TAGS = {
    PageElementTag.TAG_HTML_FONT,
  };

  /**
   * @return Tags to look for.
   */
  @Override
  protected String[] getTags() {
    return TAGS;
  }
}
