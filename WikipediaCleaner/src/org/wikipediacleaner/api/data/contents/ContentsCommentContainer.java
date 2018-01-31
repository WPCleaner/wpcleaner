/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.List;


/**
 * Container for comments elements.
 */
public class ContentsCommentContainer extends
    ContentsContainer<ContentsComment> {

  /**
   * @param comments List of comments.
   */
  public ContentsCommentContainer(List<ContentsComment> comments) {
    super(comments);
  }
}
