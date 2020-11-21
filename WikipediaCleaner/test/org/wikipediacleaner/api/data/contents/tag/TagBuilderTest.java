/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.tag;

import static org.junit.Assert.*;

import org.junit.Test;


/**
 * Test class for comments builder.
 */
public class TagBuilderTest {

  @Test
  public void simpleOpeningTag() {

    // Create tags
    String tag = TagBuilder.from("TAG_NAME", TagFormat.OPEN).toString();
    String tag2 = TagBuilder.from("TAG_NAME", false, false).toString();

    // Check tags
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME>", tag);
    assertNotNull(
        "tag2 is null",
        tag2);
    assertEquals(
        "Tag2 is incorrect",
        "<TAG_NAME>", tag2);
  }

  @Test
  public void openingTagWithAttributes() {

    // Create tag
    String tag = TagBuilder
        .from("TAG_NAME", TagFormat.OPEN)
        .addAttribute("attr1", "value1")
        .addAttribute("attr2", null)
        .toString();

    // Check tag
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME attr1=\"value1\" attr2>", tag);
  }

  @Test
  public void simpleClosingTag() {

    // Create tags
    String tag = TagBuilder.from("TAG_NAME", TagFormat.CLOSE).toString();
    String tag2 = TagBuilder.from("TAG_NAME", true, false).toString();

    // Check tags
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "</TAG_NAME>", tag);
    assertNotNull(
        "tag2 is null",
        tag2);
    assertEquals(
        "Tag2 is incorrect",
        "</TAG_NAME>", tag2);
  }

  @Test
  public void closingTagWithAttributes() {

    // Create tag
    String tag = TagBuilder
        .from("TAG_NAME", TagFormat.CLOSE)
        .addAttribute("attr1", "value1")
        .addAttribute("attr2", null)
        .toString();

    // Check tag
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "</TAG_NAME>", tag);
  }

  @Test
  public void simpleFullTag() {

    // Create tags
    String tag = TagBuilder.from("TAG_NAME", TagFormat.FULL).toString();
    String tag2 = TagBuilder.from("TAG_NAME", true, true).toString();

    // Check tags
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME />", tag);
    assertNotNull(
        "tag2 is null",
        tag2);
    assertEquals(
        "Tag2 is incorrect",
        "<TAG_NAME />", tag2);
  }

  @Test
  public void fullTagWithAttributes() {

    // Create tag
    String tag = TagBuilder
        .from("TAG_NAME", TagFormat.FULL)
        .addAttribute("attr1", "value1")
        .addAttribute("attr2", null)
        .toString();

    // Check tag
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME attr1=\"value1\" attr2 />", tag);
  }
}
