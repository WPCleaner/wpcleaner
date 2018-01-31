/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.List;


/**
 * Element of type Comment.
 */
public class ContentsComment extends ContentsElement {

  /** Marker for beginning a comment */
  public static final String START = "<!--";

  /** Marker for ending a comment */
  public static final String END = "-->";

  /** Trimmed comment */
  private final String comment;

  /**
   * @param contents Contents of the page.
   * @param interval Interval on which the element is spanning.
   */
  ContentsComment(Contents contents, ContentsInterval interval) {
    super(contents, interval);
    this.comment = ContentsUtil.trimWhitespace(contents.substring(
        interval.getBeginIndex() + START.length(),
        interval.getEndIndex() - END.length()));
  }

  /**
   * @return Inside of the comment.
   */
  public String getComment() {
    return comment;
  }

  /**
   * @param comments List of comments.
   * @param text Text.
   * @param offset Index of the start of the text.
   * @return Text stripped of its comments.
   */
  @Deprecated
  // TODO: Remove method for a more general approach
  public static String stripComments(
      List<ContentsComment> comments,
      String text, int offset) {
    if ((comments == null) || (text == null)) {
      return text;
    }
    for (ContentsComment comment : comments) {
      if ((comment.getBeginIndex() < offset + text.length()) &&
          (comment.getEndIndex() > offset)) {
        int startComment = Math.max(comment.getBeginIndex() - offset, 0);
        int endComment = Math.min(comment.getEndIndex() - offset, text.length());
        String tmpText = "";
        if (startComment > 0) {
          tmpText += text.substring(0, startComment);
        }
        if (endComment < text.length()) {
          tmpText += text.substring(endComment);
        }
        text = tmpText;
        offset += endComment - startComment;
      }
    }
    return text;
  }
}
