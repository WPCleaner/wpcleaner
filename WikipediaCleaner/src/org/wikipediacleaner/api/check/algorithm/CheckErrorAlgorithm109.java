/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 109 of check wikipedia project.
 * Error 109: Include tag error
 */
public class CheckErrorAlgorithm109 extends CheckErrorAlgorithmUnclosedTags {

  /** List of tags managed by this error. */
  private final List<String> tags;

  public CheckErrorAlgorithm109() {
    super("Include tag error");
    tags = new ArrayList<String>();
    tags.add(PageElementTag.TAG_WIKI_INCLUDEONLY);
    tags.add(PageElementTag.TAG_WIKI_NOINCLUDE);
    tags.add(PageElementTag.TAG_WIKI_ONLYINCLUDE);
  }

  /**
   * @return List of tags managed by this error.
   */
  @Override
  protected List<String> getTags() {
    return tags;
  }

  /**
   * @return True if full tags should be reported.
   */
  @Override
  protected boolean reportFullTags() {
    return true;
  }
}
