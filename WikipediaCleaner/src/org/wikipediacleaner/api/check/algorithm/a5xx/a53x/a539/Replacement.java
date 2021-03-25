/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a539;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;

/**
 * Bean for holding configuration for replacements.
 */
class Replacement {

  /** First tag: surrounding */
  final TagType firstTag;

  /** Second tag: should be inside */
  final List<ReplacementElement> elements;

  /** Formatting order */
  final OrderFormatting orderFormatting;

  /** Possible replacements */
  final static Replacement[] REPLACEMENTS = {
    new Replacement(
        HtmlTagType.BIG,
        Stream.of(
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.CENTER,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.DIV, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.FONT, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.S, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUB, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUP, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.U, true, Order.MUST_KEEP)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_INSIDE),
    new Replacement(
        HtmlTagType.DIV,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.FONT, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.S, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUB, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUP, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.U, true, Order.MUST_KEEP)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_INSIDE),
    new Replacement(
        HtmlTagType.FONT,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.S,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SMALL,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SPAN,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SUB,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SUP,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.U,
        Stream.of(
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE)).collect(Collectors.toList()),
        OrderFormatting.FORMATTING_ANYWHERE),
  };

  /**
   * @param firstTag Surrounding tag type.
   * @param elements Possible tags inside.
   * @param orderFormatting Formatting order.
   */
  Replacement(
      TagType firstTag,
      List<ReplacementElement> elements,
      OrderFormatting orderFormatting) {
    this.firstTag = firstTag;
    this.elements = Collections.unmodifiableList(elements);
    this.orderFormatting = orderFormatting;
  }

  ReplacementElement getSecondTag(final TagType tag) {
    return elements.stream().filter(e -> e.tag.equals(tag)).findFirst().orElse(null);
  }
}