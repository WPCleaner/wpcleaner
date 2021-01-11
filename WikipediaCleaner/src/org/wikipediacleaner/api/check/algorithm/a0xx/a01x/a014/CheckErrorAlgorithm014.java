/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a01x.a014;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmUnclosedTags;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 14 of check wikipedia project.
 * Error 14: Source not correct end
 */
public class CheckErrorAlgorithm014 extends CheckErrorAlgorithmUnclosedTags {

  /** List of tags managed by this error. */
  private static final List<TagType> TAGS;

  static {
    List<TagType> tmpList = new ArrayList<>();
    tmpList.add(WikiTagType.SOURCE);
    tmpList.add(WikiTagType.SYNTAXHIGHLIGHT);
    TAGS = Collections.unmodifiableList(tmpList);
  }

  public CheckErrorAlgorithm014() {
    super("Source not correct end");
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
