/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a04x.a041;

import java.util.Collections;
import java.util.Set;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


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
  private final static Set<TagType> TAGS = Collections.singleton(HtmlTagType.BIG);

  /**
   * @return Tags to look for.
   */
  @Override
  protected Set<TagType> getTags() {
    return TAGS;
  }

  /**
   * @return True if complete tags should be reported as one tag instead of separate tags.
   */
  @Override
  protected boolean reportCompleteTags() {
    return false;
  }
}
