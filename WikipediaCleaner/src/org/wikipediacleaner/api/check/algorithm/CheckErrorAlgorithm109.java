/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 109 of check wikipedia project.
 * Error 109: Include tag error
 */
public class CheckErrorAlgorithm109 extends CheckErrorAlgorithmUnclosedTags {

  /** List of tags managed by this error. */
  private static final List<TagType> TAGS;

  static {
    List<TagType> tmpList = new ArrayList<>();
    tmpList.add(WikiTagType.INCLUDEONLY);
    tmpList.add(WikiTagType.NOINCLUDE);
    tmpList.add(WikiTagType.ONLYINCLUDE);
    TAGS = Collections.unmodifiableList(tmpList);
  }

  public CheckErrorAlgorithm109() {
    super("Include tag error");
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
