/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.wikipediacleaner.api.data.PageElementTag;

/**
 * Builder class.
 */
public class ContentsTagBuilder {

  // Useful prepared tags
  public static final String B_OPEN = from(PageElementTag.TAG_HTML_B, ContentsTagFormat.OPEN).toString();
  public static final String B_CLOSE = from(PageElementTag.TAG_HTML_B, ContentsTagFormat.CLOSE).toString();

  public static final String LI_OPEN = from(PageElementTag.TAG_HTML_LI, ContentsTagFormat.OPEN).toString();
  public static final String LI_CLOSE = from(PageElementTag.TAG_HTML_LI, ContentsTagFormat.CLOSE).toString();

  public static final String NOWIKI_OPEN = from(PageElementTag.TAG_WIKI_NOWIKI, ContentsTagFormat.OPEN).toString();
  public static final String NOWIKI_CLOSE = from(PageElementTag.TAG_WIKI_NOWIKI, ContentsTagFormat.CLOSE).toString();

  public static final String REF_OPEN = from(PageElementTag.TAG_WIKI_REF, ContentsTagFormat.OPEN).toString();
  public static final String REF_CLOSE = from(PageElementTag.TAG_WIKI_REF, ContentsTagFormat.CLOSE).toString();

  public static final String SMALL_OPEN = from(PageElementTag.TAG_HTML_SMALL, ContentsTagFormat.OPEN).toString();
  public static final String SMALL_CLOSE = from(PageElementTag.TAG_HTML_SMALL, ContentsTagFormat.CLOSE).toString();

  public static final String UL_OPEN = from(PageElementTag.TAG_HTML_UL, ContentsTagFormat.OPEN).toString();
  public static final String UL_CLOSE = from(PageElementTag.TAG_HTML_UL, ContentsTagFormat.CLOSE).toString();

  /** Name of the tag */
  @Nonnull
  private final String name;

  /** Format of the tag */
  @Nonnull
  private final ContentsTagFormat format;

  /** List of attributes */
  @Nonnull
  private final List<ImmutablePair<String, String>> attributes;

  /**
   * Private constructor.
   * 
   * @param tagName Name of the tag.
   * @param format Format of the tag.
   */
  private ContentsTagBuilder(@Nonnull String name, @Nonnull ContentsTagFormat format) {
    this.name = name;
    this.format = format;
    this.attributes = new ArrayList<>();
  }

  /**
   * Initialize a builder with the name of the tag.
   * 
   * @param tagName Name of the tag.
   * @param format Format of the tag.
   * @return Builder initialized with the name and format of the tag.
   */
  public static @Nonnull ContentsTagBuilder from(@Nonnull String tagName, @Nonnull ContentsTagFormat format) {
    ContentsTagBuilder builder = new ContentsTagBuilder(tagName, format);
    return builder;
  }

  /**
   * Initialize a builder with the name of the tag.
   * 
   * @param tagName Name of the tag.
   * @param closing True if it's a closing tag.
   * @param full True if it's a full tag.
   * @return Builder initialized with the name and format of the tag.
   */
  public static @Nonnull ContentsTagBuilder from(@Nonnull String tagName, boolean closing, boolean full) {
    return from(
        tagName,
        full ? ContentsTagFormat.FULL : closing ? ContentsTagFormat.CLOSE : ContentsTagFormat.OPEN);
  }

  /**
   * Add an attribute to the builder.
   * 
   * @param attributeName Name of the attribute.
   * @param attributeValue Value of the attribute.
   * @return Builder with the added attribute.
   */
  public @Nonnull ContentsTagBuilder addAttribute(@Nonnull String attributeName, @Nullable String attributeValue) {
    attributes.add(new ImmutablePair<>(attributeName, attributeValue));
    return this;
  }

  /**
   * @return Textual representation of the comment.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append('<');
    if (ContentsTagFormat.CLOSE == format) {
      sb.append('/');
    }
    sb.append(name);
    if (ContentsTagFormat.CLOSE != format) {
      for (ImmutablePair<String, String> attribute : attributes) {
        sb.append(' ');
        sb.append(attribute.getLeft());
        if (attribute.getValue() != null) {
          sb.append("=\"");
          sb.append(attribute.getRight());
          sb.append("\"");
        }
      }
    }
    if (ContentsTagFormat.FULL == format) {
      sb.append(" /");
    }
    sb.append('>');
    return sb.toString();
  }
}
