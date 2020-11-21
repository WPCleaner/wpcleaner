/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.impl;

import java.util.Map;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;

/**
 * Class to manage comments for edits.
 */
public final class CommentManager {

  /** Potential supplementary text to be added to the comments */
  private static String extraText = null;

  /**
   * Manage comment.
   * 
   * @param configuration Configuration applied.
   * @param properties Properties for the API Request.
   * @param propertyComment Name of the property for the comment.
   * @param propertyTag Name of the property for the tag.
   * @param automatic True if the modification is automatic.
   */
  public static void manageComment(
      WPCConfiguration configuration,
      Map<String, String> properties,
      String propertyComment, String propertyTag,
      boolean automatic) {

    // Various checks
    if (properties == null) {
      return;
    }

    // Tag the modification
    boolean tagUsed = false;
    if ((propertyTag != null) && (propertyTag.trim().length() > 0)) {
      String tag = configuration.getString(WPCConfigurationString.TAG);
      if ((tag != null) && (tag.length() > 0)) {
        properties.put(propertyTag.trim(), tag);
        tagUsed = true;
      }
    }

    StringBuilder updatedComment = new StringBuilder();

    // Add the information about the program name in the comment
    String programName = Version.PROGRAM;
    if (!tagUsed && (programName != null) && (programName.trim().length() > 0)) {
      String link = configuration.getString(WPCConfigurationString.HELP_PAGE);
      if ((link != null) && (link.trim().length() > 0)) {
        updatedComment.append(InternalLinkBuilder.from(link.trim()).withText(programName.trim()).toString());
      } else {
        updatedComment.append(programName.trim());
      }
    }

    // Add the information about the version of the program in the comment
    String version = Version.VERSION;
    if ((version != null) && (version.trim().length() > 0)) {
      if (updatedComment.length() > 0) {
        updatedComment.append(" ");
      }
      updatedComment.append("v");
      updatedComment.append(version.trim());
      if (automatic) {
        updatedComment.append("b");
      }
    }

    // Add a supplementary text in the comment
    if ((extraText != null) && (extraText.trim().length() > 0)) {
      if (updatedComment.length() > 0) {
        updatedComment.append(" - ");
      }
      updatedComment.append(extraText.trim());
    }

    // Add the actual comment
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

  /**
   * @param text Supplementary text to be added to the comments
   */
  public static void addExtraText(String text) {
    extraText = text;
  }
}
