/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 110 of check wikipedia project.
 * Error 110: Include tags
 */
public class CheckErrorAlgorithm110 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm110() {
    super("Include tags");
  }

  /**
   * Tags to look for.
   */
  private final static String[] TAGS = {
    PageElementTag.TAG_WIKI_INCLUDEONLY,
    PageElementTag.TAG_WIKI_NOINCLUDE,
    PageElementTag.TAG_WIKI_ONLYINCLUDE,
  };

  /**
   * @return Tags to look for.
   */
  @Override
  protected String[] getTags() {
    return TAGS;
  }
}
