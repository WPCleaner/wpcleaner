/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


/**
 * Algorithm for analyzing error 98 of check wikipedia project.
 * Error 98: Subscript not correct end
 */
public class CheckErrorAlgorithm098 extends CheckErrorAlgorithmUnclosedTags {

  /** List of tags managed by this error. */
  private static final List<TagType> TAGS = Collections.singletonList(HtmlTagType.SUB);

  public CheckErrorAlgorithm098() {
    super("Subscript not correct end");
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
