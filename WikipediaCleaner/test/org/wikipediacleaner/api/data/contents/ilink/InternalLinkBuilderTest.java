/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.ilink;

import static org.junit.Assert.*;

import org.junit.Test;


/**
 * Test class for comments builder.
 */
public class InternalLinkBuilderTest {

  @Test
  public void simpleInternalLink() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[TARGET]]", link);
  }

  @Test
  public void internalLinkWithText() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").withText("TEXT").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[TARGET|TEXT]]", link);
  }

  @Test
  public void allNull() {

    // Create internal link
    String link = InternalLinkBuilder.from(null).toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[]]", link);
  }

  @Test
  public void nullLinkAndEmptyText() {

    // Create internal link
    String link = InternalLinkBuilder.from(null).withText("").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[|]]", link);
  }

  @Test
  public void emptyLinkAndNullText() {

    // Create internal link
    String link = InternalLinkBuilder.from("").withText(null).toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[]]", link);
  }

  @Test
  public void allEmpty() {

    // Create internal link
    String link = InternalLinkBuilder.from("").withText("").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[|]]", link);
  }

  @Test
  public void internalLinkWithAnchor() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").withAnchor("ANCHOR").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[TARGET#ANCHOR]]", link);
  }

  @Test
  public void internalLinkWithColon() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").withColon(true).toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[:TARGET]]", link);
  }

  @Test
  public void sameLinkAndText() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").withText("TARGET").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[TARGET]]", link);
  }

  @Test
  public void compatibleLinkAndText() {

    // Create internal link
    String link = InternalLinkBuilder.from("Target").withText("target").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[target]]", link);
  }

  @Test
  public void textLongerThanLink() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").withText("TARGETBIS").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[TARGET]]BIS", link);
  }

  @Test
  public void textLongerThanLinkWithSpace() {

    // Create internal link
    String link = InternalLinkBuilder.from("TARGET").withText("TARGETBIS TER").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[TARGET|TARGETBIS TER]]", link);
  }

  @Test
  public void compatibleFullLinkAndText() {

    // Create internal link
    String link = InternalLinkBuilder.from("Target").withAnchor("ANCHOR").withText("target#ANCHOR").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[target#ANCHOR]]", link);
  }

  @Test
  public void completeAndCompatible() {

    // Create internal link
    String link = InternalLinkBuilder.from("Target").withAnchor("ANCHOR").withColon(true).withText(":Target#ANCHORBIS").toString();

    // Check internal link
    assertNotNull(
        "link is null",
        link);
    assertEquals(
        "Link is incorrect",
        "[[:Target#ANCHOR]]BIS", link);
  }
}
