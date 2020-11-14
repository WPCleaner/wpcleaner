/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;

/**
 * Builder class.
 */
public class ContentsFullTagBuilder {

  /** Builder for the opening tag */
  @Nonnull
  private final ContentsTagBuilder openingTagBuilder;

  /** Builder for the closing tag */
  @Nonnull
  private final ContentsTagBuilder closingTagBuilder;

  /** Contents between the opening and closing tags */
  @Nonnull
  private final String contents;

  /**
   * Private constructor.
   * 
   * @param tagName Name of the tag.
   * @param contents Contents between the opening and closing tags.
   */
  private ContentsFullTagBuilder(@Nonnull String name, @Nullable String contents) {
    this.openingTagBuilder = ContentsTagBuilder.from(name, ContentsTagFormat.OPEN);
    this.closingTagBuilder = ContentsTagBuilder.from(name, ContentsTagFormat.CLOSE);
    this.contents = StringUtils.defaultIfEmpty(contents, "");
  }

  /**
   * Initialize a builder with the name of the tag.
   * 
   * @param tagName Name of the tag.
   * @param contents Contents between the opening and closing tags.
   * @return Builder initialized with the name and format of the tag.
   */
  public static @Nonnull ContentsFullTagBuilder from(@Nonnull String tagName, @Nullable String contents) {
    ContentsFullTagBuilder builder = new ContentsFullTagBuilder(tagName, contents);
    return builder;
  }

  /**
   * Add an attribute to the builder.
   * 
   * @param attributeName Name of the attribute.
   * @param attributeValue Value of the attribute.
   * @return Builder with the added attribute.
   */
  public @Nonnull ContentsFullTagBuilder addAttribute(@Nonnull String attributeName, @Nullable String attributeValue) {
    openingTagBuilder.addAttribute(attributeName, attributeValue);
    return this;
  }

  /**
   * @return Textual representation of the comment.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(openingTagBuilder.toString());
    sb.append(contents);
    sb.append(closingTagBuilder.toString());
    return sb.toString();
  }
}
