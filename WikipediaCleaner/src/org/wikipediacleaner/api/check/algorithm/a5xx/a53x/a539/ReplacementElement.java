/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a539;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.data.contents.tag.TagType;

/**
 * Bean for holding configuration for a replacement.
 */
class ReplacementElement {

  /** Tag */
  final TagType tag;

  /** True if replacement can be automatic */
  final boolean automatic;

  /** Possibilities for order of tags */
  final Order order;

  /**
   * @param tagType Included tag type.
   * @param automatic Automatic replacement.
   * @order Possibilities for order of tags.
   */
  ReplacementElement(
      @Nonnull TagType tagType,
      boolean automatic,
      Order order) {
    this.tag = tagType;
    this.automatic = automatic;
    this.order = order;
  }
}