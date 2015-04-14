/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

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

  /**
   * @param comments List of comments.
   * @param text Text.
   * @param offset Index of the start of the text.
   * @return Text stripped of its comments.
   */
  public static String stripComments(
      List<PageElementComment> comments,
      String text, int offset) {
    if ((comments == null) || (text == null)) {
      return text;
    }
    for (PageElementComment comment : comments) {
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

  /**
   * @param beginIndex Begin index for the comment.
   * @param endIndex End index for the comment.
   * @param comment Comment contents.
   */
  private PageElementComment(
      int beginIndex, int endIndex,
      String comment) {
    super(beginIndex, endIndex);
    this.commentNotTrimmed = comment;
    this.comment = (comment != null) ? comment.trim() : null;
  }

  /**
   * @return Comment.
   */
  public String getComment() {
    return comment;
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
