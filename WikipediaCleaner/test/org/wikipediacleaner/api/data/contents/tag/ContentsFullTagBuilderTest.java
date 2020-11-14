/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.tag;

import static org.junit.Assert.*;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;


/**
 * Test class for comments builder.
 */
public class ContentsFullTagBuilderTest {

  @Test
  public void simpleTagWithNullContent() {

    // Create comment
    String tag = ContentsFullTagBuilder.from("TAG_NAME", null).toString();

    // Check comment
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME></TAG_NAME>", tag);
  }

  @Test
  public void tagWithAttributesAndNullContent() {

    // Create comment
    String tag = ContentsFullTagBuilder.from("TAG_NAME", null).addAttribute("attr1", "value1").toString();

    // Check comment
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME attr1=\"value1\"></TAG_NAME>", tag);
  }

  @Test
  public void simpleTagWithEmptyContent() {

    // Create comment
    String tag = ContentsFullTagBuilder.from("TAG_NAME", StringUtils.EMPTY).toString();

    // Check comment
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME></TAG_NAME>", tag);
  }

  @Test
  public void tagWithAttributesAndEmptyContent() {

    // Create comment
    String tag = ContentsFullTagBuilder.from("TAG_NAME", StringUtils.EMPTY).addAttribute("attr1", "value1").toString();

    // Check comment
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME attr1=\"value1\"></TAG_NAME>", tag);
  }

  @Test
  public void simpleTagWithContent() {

    // Create comment
    String tag = ContentsFullTagBuilder.from("TAG_NAME", "Tag content").toString();

    // Check comment
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME>Tag content</TAG_NAME>", tag);
  }

  @Test
  public void tagWithAttributesAndContent() {

    // Create comment
    String tag = ContentsFullTagBuilder.from("TAG_NAME", "Tag content").addAttribute("attr1", "value1").toString();

    // Check comment
    assertNotNull(
        "tag is null",
        tag);
    assertEquals(
        "Tag is incorrect",
        "<TAG_NAME attr1=\"value1\">Tag content</TAG_NAME>", tag);
  }
}
