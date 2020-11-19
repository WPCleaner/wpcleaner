/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.title;

import static org.junit.Assert.*;

import org.junit.Test;


/**
 * Test class for comments builder.
 */
public class ContentsTitleBuilderTest {

  @Test
  public void simpleTitle() {

    // Create title
    String title = ContentsTitleBuilder.from(1, "TITLE").toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "= TITLE =", title);
  }

  @Test
  public void simpleTitleOtherLevel() {

    // Create title
    String title = ContentsTitleBuilder.from(3, "TITLE").toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "=== TITLE ===", title);
  }

  @Test
  public void simpleTitleUnbalancedLevel() {

    // Create title
    String title = ContentsTitleBuilder.from(3, "TITLE").withSecondLevel(2).toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "=== TITLE ==", title);
  }

  @Test
  public void simpleNoTrim() {

    // Create title
    String title = ContentsTitleBuilder.from(3, "  TITLE").withTrimTitle(false).toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "===  TITLE===", title);
  }

  @Test
  public void nullTitle() {

    // Create title
    String title = ContentsTitleBuilder.from(3, null).toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "=== ===", title);
  }

  @Test
  public void emptyAfter() {

    // Create title
    String title = ContentsTitleBuilder.from(3, "TITLE").withAfter("").toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "=== TITLE ===", title);
  }

  @Test
  public void withAfter() {

    // Create title
    String title = ContentsTitleBuilder.from(3, "TITLE").withAfter("AFTER").toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "=== TITLE === AFTER", title);
  }

  @Test
  public void withAfterNoTrim() {

    // Create title
    String title = ContentsTitleBuilder.from(3, "TITLE").withAfter("AFTER ").withTrimAfter(false).toString();

    // Check title
    assertNotNull(
        "title is null",
        title);
    assertEquals(
        "Title is incorrect",
        "=== TITLE ===AFTER ", title);
  }
}
