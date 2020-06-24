/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents;

import static org.junit.Assert.*;

import org.junit.Test;
import org.wikipediacleaner.api.data.analysis.Contents;


/**
 * Test class for comments inside contents.
 */
public class ContentsCommentTest {

  /**
   * Simple test on a text without comments.
   */
  @Test
  public void testWithoutComments() {

    // Create contents
    String text =
        "This is a simple text, without any comments.\n\n" +
        "It goes on several lines.";
    Contents contents = Contents.createContents(text);
    assertNotNull(
        "Contents object is null",
        contents);
    assertEquals(
        "Contents is different than provided text",
        text, contents.getText());

    // Check comments container
    ContainerComment container = contents.comments();
    assertNotNull(
        "Comments container is null",
        container);
    assertNotNull(
        "List of comments is null",
        container.getAll());
    assertEquals(
        "List of comments is not empty",
        0, container.getAll().size());
  }

  /**
   * Simple test on a text with one comment.
   */
  @Test
  public void testWithOneComment() {

    // Create contents
    String text1 = "This is a simple text, without one comment.\n\nThis is a comment: ";
    String text2a = ContentsComment.START;
    String text2b = "This is a comment";
    String text2c = ContentsComment.END;
    String text3 = "This is after the comment.\n\n";
    String text = text1 + text2a + text2b + text2c + text3;
    Contents contents = Contents.createContents(text);
    assertNotNull(
        "Contents object is null",
        contents);
    assertEquals(
        "Contents is different than provided text",
        text, contents.getText());

    // Check comments container
    ContainerComment container = contents.comments();
    assertNotNull(
        "Comments container is null",
        container);
    assertNotNull(
        "List of comments is null",
        container.getAll());
    assertEquals(
        "List of comments is not a singleton",
        1, container.getAll().size());

    // Check comment
    ContentsComment comment = container.getAll().get(0);
    assertNotNull(
        "Comment is null",
        comment);
    assertNotNull(
        "Comment interval is null",
        comment.getInterval());
    assertEquals(
        "Comment doesn't start at expected index",
        text1.length(), comment.getBeginIndex());
    assertEquals(
        "Comment doesn't end at expected index",
        text1.length() + text2a.length() + text2b.length() + text2c.length(),
        comment.getEndIndex());
    assertEquals(
        "Comment value is incorrect",
        text2b, comment.getComment());
  }

  /**
   * Simple test on a text with two comments.
   */
  @Test
  public void testWithTwoComments() {

    // Create contents
    String text1 = "This is a simple text, without one comment.\n\nThis is a comment: ";
    String text2a = ContentsComment.START;
    String text2b = "This is a comment";
    String text2c = ContentsComment.END;
    String text3 = "This is beteween the comments";
    String text4a = ContentsComment.START;
    String text4b = "This is a second comment";
    String text4c = ContentsComment.END;
    String text5 = "This is after the comments.\n\n";
    String text = text1 + text2a + text2b + text2c + text3 + text4a + text4b + text4c + text5;
    Contents contents = Contents.createContents(text);
    assertNotNull(
        "Contents object is null",
        contents);
    assertEquals(
        "Contents is different than provided text",
        text, contents.getText());

    // Check comments container
    ContainerComment container = contents.comments();
    assertNotNull(
        "Comments container is null",
        container);
    assertNotNull(
        "List of comments is null",
        container.getAll());
    assertEquals(
        "List of comments is not with 2 elements",
        2, container.getAll().size());

    // Check first comment
    ContentsComment comment = container.getAll().get(0);
    assertNotNull(
        "Comment is null",
        comment);
    assertNotNull(
        "Comment interval is null",
        comment.getInterval());
    int beginIndex = text1.length();
    assertEquals(
        "Comment doesn't start at expected index",
        beginIndex , comment.getBeginIndex());
    int endIndex = beginIndex + text2a.length() + text2b.length() + text2c.length();
    assertEquals(
        "Comment doesn't end at expected index",
        endIndex, comment.getEndIndex());
    assertEquals(
        "Comment value is incorrect",
        text2b, comment.getComment());

    // Check second comment
    comment = container.getAll().get(1);
    assertNotNull(
        "Comment is null",
        comment);
    assertNotNull(
        "Comment interval is null",
        comment.getInterval());
    beginIndex = text1.length() + text2a.length() + text2b.length() + text2c.length() + text3.length();
    assertEquals(
        "Comment doesn't start at expected index",
        beginIndex, comment.getBeginIndex());
    endIndex = beginIndex + text4a.length() + text4b.length() + text4c.length();
    assertEquals(
        "Comment doesn't end at expected index",
        endIndex, comment.getEndIndex());
    assertEquals(
        "Comment value is incorrect",
        text4b, comment.getComment());
  }
}
