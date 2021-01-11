/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;


/**
 * Algorithm for analyzing error 31 of check wikipedia project.
 * Error 31: HTML table element
 */
public class CheckErrorAlgorithm031 extends CheckErrorAlgorithmTags {

  public CheckErrorAlgorithm031() {
    super("HTML table element");
  }

  /**
   * Tags to look for.
   */
  private final static Set<TagType> TAGS;

  static {
	Set<TagType> tmpTags = new HashSet<>();
    tmpTags.add(HtmlTagType.TABLE);
    tmpTags.add(HtmlTagType.TD);
    tmpTags.add(HtmlTagType.TH);
    tmpTags.add(HtmlTagType.TR);
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
    return false;
  }
}
