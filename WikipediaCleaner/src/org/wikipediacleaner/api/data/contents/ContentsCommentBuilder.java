/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;


/**
 * Builder class.
 */
public class ContentsCommentBuilder {

  /** Text of the comment */
  private String comment;

  /**
   * Private constructor.
   */
  private ContentsCommentBuilder() {
    // Nothing to do
  }

  /**
   * Initialize a builder with the text of the comment.
   * 
   * @param comment Text of the comment.
   * @return Builder initialized with the text of the comment.
   */
  public static ContentsCommentBuilder from(String comment) {
    ContentsCommentBuilder builder = new ContentsCommentBuilder();
    builder.comment = comment;
    return builder;
  }

  /**
   * @return Textual representation of the comment.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(ContentsComment.START);
    sb.append(' ');
    sb.append(ContentsUtil.trimWhitespace(comment));
    sb.append(' ');
    sb.append(ContentsComment.END);
    return sb.toString();
  }
}
