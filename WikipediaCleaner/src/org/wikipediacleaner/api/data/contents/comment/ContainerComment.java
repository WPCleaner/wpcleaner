/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.comment;

import java.util.List;

import org.wikipediacleaner.api.data.contents.ContainerBehavior;
import org.wikipediacleaner.api.data.contents.ContainerContents;


/**
 * Container for comments elements.
 */
public class ContainerComment extends
    ContainerContents<ContentsComment> {

  /**
   * @param comments List of comments.
   */
  public ContainerComment(List<ContentsComment> comments) {
    super(comments, ContainerBehavior.LARGEST_ONLY);
  }
}
