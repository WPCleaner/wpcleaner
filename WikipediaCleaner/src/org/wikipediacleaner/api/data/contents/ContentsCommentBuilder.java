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

  /**
   * @param comment Comment.
   * @return Textual representation of the comment.
   */
  public static String create(String comment) {
    StringBuilder sb = new StringBuilder();
    sb.append(ContentsComment.START);
    sb.append(' ');
    sb.append(ContentsUtil.trimWhitespace(comment));
    sb.append(' ');
    sb.append(ContentsComment.END);
    return sb.toString();
  }
}
