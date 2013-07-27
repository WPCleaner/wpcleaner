/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a comment (<!-- Comment -->). 
 */
public class PageElementComment extends PageElement {

  private final String commentNotTrimmed;
  private final String comment;

  /**
   * Analyze contents to check if it matches a comment.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementComment analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }
    if ((index < 0) || (index >= contents.length())) {
      return null;
    }

    // Check that it starts as a comment
    if (!contents.startsWith("<!--", index)) {
      return null;
    }
    int beginComment = index + 4;

    // Check that the comment ends
    int endIndex = contents.indexOf("-->", beginComment);
    if (endIndex < 0) {
      return null;
    }

    return new PageElementComment(
        index, endIndex + 3,
        contents.substring(beginComment, endIndex));
  }

  public String getComment() {
    return comment;
  }

  private PageElementComment(
      int beginIndex, int endIndex,
      String comment) {
    super(beginIndex, endIndex);
    this.commentNotTrimmed = comment;
    this.comment = (comment != null) ? comment.trim() : null;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return createComment(commentNotTrimmed);
  }

  /**
   * Create a comment.
   * 
   * @param comment Comment.
   * @return Comment.
   */
  public static String createComment(String comment) {
    StringBuilder sb = new StringBuilder();
    sb.append("<!-- ");
    if (comment != null) {
      sb.append(comment.trim());
    }
    sb.append(" -->");
    return sb.toString();
  }
}
