/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a04x.a040;

import java.util.Collections;
import java.util.Set;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


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
  private final static Set<TagType> TAGS = Collections.singleton(HtmlTagType.FONT);

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
