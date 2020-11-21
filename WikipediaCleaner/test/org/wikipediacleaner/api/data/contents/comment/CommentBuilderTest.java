/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.comment;

import static org.junit.Assert.*;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;


/**
 * Test class for comments builder.
 */
public class CommentBuilderTest {

  /**
   * Null comment.
   */
  @Test
  public void testWithNullComment() {

    // Create comment
    String comment = CommentBuilder.from(null).toString();

    // Check comment
    assertNotNull(
        "Comment is null",
        comment);
    assertEquals(
        "Comment is incorrect",
        "<!-- -->", comment);
  }

  /**
   * Empty comment.
   */
  @Test
  public void testWithEmptyComment() {

    // Create comment
    String comment = CommentBuilder.from(StringUtils.EMPTY).toString();

    // Check comment
    assertNotNull(
        "Comment is null",
        comment);
    assertEquals(
        "Comment is incorrect",
        "<!-- -->", comment);
  }

  /**
   * Normal comment.
   */
  @Test
  public void testWithNormalComment() {

    // Create comment
    String comment = CommentBuilder.from("This is a comment").toString();

    // Check comment
    assertNotNull(
        "Comment is null",
        comment);
    assertEquals(
        "Comment is incorrect",
        "<!-- This is a comment -->", comment);
  }
}
