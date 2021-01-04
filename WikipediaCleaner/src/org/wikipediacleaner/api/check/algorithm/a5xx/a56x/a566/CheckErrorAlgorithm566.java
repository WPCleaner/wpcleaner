/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a566;

import java.util.HashSet;
import java.util.Set;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags2;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 566 of check wikipedia project.
 * Error 566: abbr tags
 */
public class CheckErrorAlgorithm566 extends CheckErrorAlgorithmTags2 {

  private static final @Nonnull Set<String> tags = new HashSet<>();

  static {
    tags.add(PageElementTag.TAG_HTML_ABBR);
  }

  public CheckErrorAlgorithm566() {
    super("<abbr> tags");

  }

  /**
   * @return Tags to look for.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags2#getTags()
   */
  @Override
  protected Set<String> getTags() {
    return tags;
  }

}
