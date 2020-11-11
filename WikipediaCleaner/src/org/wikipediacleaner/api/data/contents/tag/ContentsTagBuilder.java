/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.tuple.ImmutablePair;

/**
 * Builder class.
 */
public class ContentsTagBuilder {

  /** Name of the tag */
  private final String name;

  /** Format of the tag */
  private final ContentsTagFormat format;

  /** List of attributes */
  private final List<ImmutablePair<String, String>> attributes;

  /**
   * Private constructor.
   * 
   * @param tagName Name of the tag.
   * @param format Format of the tag.
   */
  private ContentsTagBuilder(String name, ContentsTagFormat format) {
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
  public static ContentsTagBuilder from(String tagName, ContentsTagFormat format) {
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
  public static ContentsTagBuilder from(String tagName, boolean closing, boolean full) {
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
  public ContentsTagBuilder addAttribute(String attributeName, String attributeValue) {
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
    for (ImmutablePair<String, String> attribute : attributes) {
      sb.append(' ');
      sb.append(attribute.getLeft());
      if (attribute.getValue() != null) {
        sb.append("=\"");
        sb.append(attribute.getRight());
        sb.append("\"");
      }
    }
    if (ContentsTagFormat.FULL == format) {
      sb.append(" /");
    }
    sb.append('>');
    return sb.toString();
  }
}
