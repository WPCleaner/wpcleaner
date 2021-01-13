/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a1xx.a11x.a110;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 110 of check wikipedia project.
 * Error 110: Include tags
 */
public class CheckErrorAlgorithm110 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm110() {
    super("Include tags");
  }

  /** Tags to look for. */
  private final static Set<TagType> TAGS;
  static {
    Set<TagType> tmpTags = new HashSet<>();
    tmpTags.add(WikiTagType.INCLUDEONLY);
    tmpTags.add(WikiTagType.NOINCLUDE);
    tmpTags.add(WikiTagType.ONLYINCLUDE);
    TAGS = Collections.unmodifiableSet(tmpTags);
  }

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
    return true;
  }
}
