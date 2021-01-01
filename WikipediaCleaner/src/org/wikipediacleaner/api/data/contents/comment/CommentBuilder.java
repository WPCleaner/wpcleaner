/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.comment;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.contents.ContentsUtil;

/**
 * Builder class.
 */
public class CommentBuilder {

  /** Text of the comment */
  @Nonnull
  private String comment;

  /** True to have whitespace separators */
  private boolean whitespace;

  /**
   * Private constructor.
   */
  private CommentBuilder(@Nonnull String comment) {
    this.comment = comment;
    whitespace = true;
  }

  /**
   * Initialize a builder with the text of the comment.
   * 
   * @param comment Text of the comment.
   * @return Builder initialized with the text of the comment.
   */
  public static @Nonnull CommentBuilder from(@Nullable String comment) {
    CommentBuilder builder = new CommentBuilder(StringUtils.defaultIfEmpty(comment, StringUtils.EMPTY));
    return builder;
  }

  /**
   * @param flag True to have whitespace separators.
   * @return Builder.
   */
  public @Nonnull CommentBuilder withWhitespace(boolean flag) {
    this.whitespace = flag;
    return this;
  }

  /**
   * @return Textual representation of the comment.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(ContentsComment.START);
    if (whitespace) {
      sb.append(' ');
    }
    sb.append(ContentsUtil
        .trimWhitespace(comment)
        .replaceAll(ContentsComment.START, "< !--")
        .replaceAll(ContentsComment.END, "-- >"));
    if ((sb.charAt(sb.length() - 1) != ' ') && whitespace) {
      sb.append(' ');
    }
    sb.append(ContentsComment.END);
    return sb.toString();
  }
}
