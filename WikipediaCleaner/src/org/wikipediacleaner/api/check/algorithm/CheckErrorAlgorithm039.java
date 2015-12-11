/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 39 of check wikipedia project.
 * Error 39: HTML text style element &lt;p&gt;
 */
public class CheckErrorAlgorithm039 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm039() {
    super("HTML text style element <p>");
  }

  /**
   * Tags to look for.
   */
  private final static String[] TAGS = {
    PageElementTag.TAG_HTML_P,
  };

  /**
   * @return Tags to look for.
   */
  @Override
  protected String[] getTags() {
    return TAGS;
  }
}
