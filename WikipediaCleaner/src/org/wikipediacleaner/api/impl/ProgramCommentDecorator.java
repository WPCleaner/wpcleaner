/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.impl;

import java.util.Map;

import org.wikipediacleaner.api.data.PageElementInternalLink;


/**
 * Change comments on the fly to add information about the program.
 */
public class ProgramCommentDecorator implements CommentDecorator {

  /** Program name. */
  private final String program;

  /** Program version. */
  private final String version;

  /** True if program name should be added in the comments. */
  private final boolean showProgram;

  /** Optional link. */
  private final String link;

  /** Tag to use. */
  private final String tag;

  /**
   * @param program Program name.
   * @param version Program version.
   * @param showProgram True if program name should be added in the comments.
   * @param link Optional link.
   * @param tag Optional tag.
   */
  public ProgramCommentDecorator(
      String program, String version,
      boolean showProgram, String link,
      String tag) {
    this.program = (program != null) ? program.trim() : "";
    this.version = (version != null) ? version.trim() : "";
    this.showProgram = showProgram;
    this.link = (link != null) ? link.trim() : "";
    this.tag = (tag != null) ? tag.trim() : "";
  }

  /**
   * Manage comment.
   * 
   * @param properties Properties for the API Request.
   * @param propertyComment Name of the property for the comment.
   * @param propertyTag Name of the property for the tag.
   * @param automatic True if the modification is automatic.
   * @see org.wikipediacleaner.api.impl.CommentDecorator#manageComment(Map, String, String, boolean)
   */
  @Override
  public void manageComment(
      Map<String, String> properties,
      String propertyComment, String propertyTag,
      boolean automatic) {
    // Tag the modification
    boolean tagUsed = false;
    if ((tag != null) && (tag.length() > 0)) {
      if ((propertyTag != null) && (propertyTag.trim().length() > 0)) {
        properties.put(propertyTag.trim(), tag);
        tagUsed = true;
      }
    }

    // Update the comment
    StringBuilder updatedComment = new StringBuilder();
    if (showProgram && !tagUsed &&
        (program != null) && (program.length() > 0)) {
      if ((link != null) && (link.length() > 0)) {
        updatedComment.append(PageElementInternalLink.createInternalLink(link, program));
      } else {
        updatedComment.append(program);
      }
    }
    if ((version != null) && (version.length() > 0)) {
      if (updatedComment.length() > 0) {
        updatedComment.append(" ");
      }
      updatedComment.append("v");
      updatedComment.append(version);
      if (automatic) {
        updatedComment.append("b");
      }
    }
    String comment = properties.get(propertyComment);
    if ((comment != null) && (comment.trim().length() > 0)) {
      if (updatedComment.length() > 0) {
        updatedComment.append(" - ");
      }
      updatedComment.append(comment);
    }
    if (updatedComment.length() > 0) {
      properties.put(propertyComment, updatedComment.toString());
    }
  }

}
