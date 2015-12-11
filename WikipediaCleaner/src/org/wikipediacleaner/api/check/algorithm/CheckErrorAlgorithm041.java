/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 41 of check wikipedia project.
 * Error 41: HTML text style element &lt;big&gt;
 */
public class CheckErrorAlgorithm041 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm041() {
    super("HTML text style element <big>");
  }

  /**
   * Tags to look for.
   */
  private final static String[] TAGS = {
    PageElementTag.TAG_HTML_BIG,
  };

  /**
   * @return Tags to look for.
   */
  @Override
  protected String[] getTags() {
    return TAGS;
  }
}
