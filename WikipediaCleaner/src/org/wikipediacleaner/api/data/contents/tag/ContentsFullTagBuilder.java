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
import org.wikipediacleaner.api.data.PageElementTag;

/**
 * Builder class.
 */
public class ContentsFullTagBuilder {

  private static final String ELLIPSIS = "...";

  // Useful prepared tags
  public static final String CENTER = from(PageElementTag.TAG_HTML_CENTER, ELLIPSIS).toString();
  public static final String NOWIKI = from(PageElementTag.TAG_WIKI_NOWIKI, ELLIPSIS).toString();
  public static final String REF = from(PageElementTag.TAG_WIKI_REF, ELLIPSIS).toString();
  public static final String SMALL = from(PageElementTag.TAG_HTML_SMALL, ELLIPSIS).toString();
  public static final String STRIKE = from(PageElementTag.TAG_HTML_STRIKE, ELLIPSIS).toString();
  public static final String TT = from(PageElementTag.TAG_HTML_TT, ELLIPSIS).toString();

  /** Builder for the opening tag */
  @Nonnull
  private final ContentsTagBuilder openingTagBuilder;

  /** Builder for the closing tag */
  @Nonnull
  private final ContentsTagBuilder closingTagBuilder;

  /** Builder for the full tag */
  @Nonnull
  private final ContentsTagBuilder fullTagBuilder;

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
  private ContentsFullTagBuilder(@Nonnull String name, @Nullable String contents) {
    this.openingTagBuilder = ContentsTagBuilder.from(name, ContentsTagFormat.OPEN);
    this.closingTagBuilder = ContentsTagBuilder.from(name, ContentsTagFormat.CLOSE);
    this.fullTagBuilder = ContentsTagBuilder.from(name, ContentsTagFormat.FULL);
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
    fullTagBuilder.addAttribute(attributeName, attributeValue);
    return this;
  }

  /**
   * Configure the builder for full tags with empty contents.
   * 
   * @param force True to force the generation of opening and closing tags even if contents is empty.
   * @return Builder with the changed configuration.
   */
  public @Nonnull ContentsFullTagBuilder withForceOpenCloseTags(boolean force) {
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
