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
public class CompleteTagBuilder {

  /** Builder for the opening tag */
  @Nonnull
  private final TagBuilder openingTagBuilder;

  /** Builder for the closing tag */
  @Nonnull
  private final TagBuilder closingTagBuilder;

  /** Builder for the full tag */
  @Nonnull
  private final TagBuilder fullTagBuilder;

  /** Contents between the opening and closing tags */
  @Nonnull
  private final String contents;

  /** True to force the creation of opening and closing tags even if contents is empty */
  private boolean forceOpenCloseTags;

  /**
   * Private constructor.
   * 
   * @param tagName Name of the tag.
   * @param contents Contents between the opening and closing tags.
   */
  private CompleteTagBuilder(@Nonnull String name, @Nullable String contents) {
    this.openingTagBuilder = TagBuilder.from(name, TagFormat.OPEN);
    this.closingTagBuilder = TagBuilder.from(name, TagFormat.CLOSE);
    this.fullTagBuilder = TagBuilder.from(name, TagFormat.FULL);
    this.contents = StringUtils.defaultIfEmpty(contents, "");
  }

  /**
   * Initialize a builder with the name of the tag.
   * 
   * @param tagName Name of the tag.
   * @param contents Contents between the opening and closing tags.
   * @return Builder initialized with the name and format of the tag.
   */
  public static @Nonnull CompleteTagBuilder from(@Nonnull String tagName, @Nullable String contents) {
    CompleteTagBuilder builder = new CompleteTagBuilder(tagName, contents);
    return builder;
  }

  /**
   * Initialize a builder with the type of the tag.
   * 
   * @param tagType Type of the tag.
   * @param contents Contents between the opening and closing tags.
   * @return Builder initialized with the name and format of the tag.
   */
  public static @Nonnull CompleteTagBuilder from(@Nonnull TagType tagType, @Nullable String contents) {
    CompleteTagBuilder builder = new CompleteTagBuilder(tagType.getNormalizedName(), contents);
    return builder;
  }

  /**
   * Add an attribute to the builder.
   * 
   * @param attributeName Name of the attribute.
   * @param attributeValue Value of the attribute.
   * @return Builder with the added attribute.
   */
  public @Nonnull CompleteTagBuilder addAttribute(@Nonnull String attributeName, @Nullable String attributeValue) {
    openingTagBuilder.addAttribute(attributeName, attributeValue);
    fullTagBuilder.addAttribute(attributeName, attributeValue);
    return this;
  }

  /**
   * Configure the builder for full tags with empty contents.
   * 
   * @param force True to force the generation of opening and closing tags even if contents is empty.
   * @return Builder with the changed configuration.
   */
  public @Nonnull CompleteTagBuilder withForceOpenCloseTags(boolean force) {
    this.forceOpenCloseTags = force;
    return this;
  }

  /**
   * @return Textual representation of the comment.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    if (!forceOpenCloseTags && StringUtils.isEmpty(contents)) {
      return fullTagBuilder.toString();
    }
    StringBuilder sb = new StringBuilder();
    sb.append(openingTagBuilder.toString());
    sb.append(contents);
    sb.append(closingTagBuilder.toString());
    return sb.toString();
  }
}
