/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.title;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;

/**
 * Builder class.
 */
public class ContentsTitleBuilder {

  /** Level of the title (number of equal signs) */
  private final int level;

  /** Different level (unbalanced level */
  private int secondLevel;

  /** Text of the title */
  @Nonnull
  private final String title;

  /** Text after the title */
  @Nonnull
  private String after;

  /** True to trim the text of the title */
  private boolean trimTitle = true;

  /** True to trim the text after the title */
  private boolean trimAfter = true;

  /**
   * Private constructor.
   * 
   * @param level Level of the title.
   * @param title Text of the title.
   */
  private ContentsTitleBuilder(int level, @Nonnull String title) {
    this.level = level;
    this.secondLevel = level;
    this.title = title;
    this.after = StringUtils.EMPTY;
  }

  /**
   * Initialize a builder with the level and text of the title.
   * 
   * @param level Level of the title.
   * @param title Text of the title.
   * @return Builder initialized with the level and text of the title.
   */
  public static @Nonnull ContentsTitleBuilder from(int level, @Nullable String title) {
    ContentsTitleBuilder builder = new ContentsTitleBuilder(level, StringUtils.defaultIfEmpty(title, StringUtils.EMPTY));
    return builder;
  }

  /**
   * @param withLevel Second level.
   * @return Builder.
   */
  public ContentsTitleBuilder withSecondLevel(int withLevel) {
    this.secondLevel = withLevel;
    return this;
  }

  /**
   * @param withTrim True if the text of the title should be trimmed.
   * @return Builder.
   */
  public ContentsTitleBuilder withTrimTitle(boolean withTrim) {
    this.trimTitle = withTrim;
    return this;
  }

  /**
   * @param withAfter Text after the link. 
   * @return Builder.
   */
  public ContentsTitleBuilder withAfter(@Nullable String withAfter) {
    this.after = StringUtils.defaultIfEmpty(withAfter, StringUtils.EMPTY);
    return this;
  }

  /**
   * @param withTrim True if the text after the title should be trimmed.
   * @return Builder.
   */
  public ContentsTitleBuilder withTrimAfter(boolean withTrim) {
    this.trimAfter = withTrim;
    return this;
  }

  /**
   * @return Textual representation of the title.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(StringUtils.repeat('=', level));
    if (trimTitle) {
      sb.append(' ');
      sb.append(title.trim());
      if (title.trim().length() > 0) {
        sb.append(' ');
      }
    } else {
      sb.append(title);
    }
    sb.append(StringUtils.repeat('=', secondLevel));
    if (StringUtils.isNotEmpty(after)) {
      if (trimAfter) {
        sb.append(' ');
        sb.append(after.trim());
      } else {
        sb.append(after);
      }
    }
    return sb.toString();
  }
}
