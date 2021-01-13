/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a09x.a099;

import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmUnclosedTags;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


/**
 * Algorithm for analyzing error 99 of check wikipedia project.
 * Error 99: Superscript not correct end
 */
public class CheckErrorAlgorithm099 extends CheckErrorAlgorithmUnclosedTags {

  /** List of tags managed by this error. */
  private static final List<TagType> TAGS = Collections.singletonList(HtmlTagType.SUP);

  public CheckErrorAlgorithm099() {
    super("Superscript not correct end");
  }

  /**
   * @return List of tags managed by this error.
   */
  @Override
  protected List<TagType> getTags() {
    return TAGS;
  }

  /**
   * @return True if full tags should be reported.
   */
  @Override
  protected boolean reportFullTags() {
    return true;
  }
}
