/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.impl;

import java.util.Map;


/**
 * Interface to change comments on the fly.
 */
public interface CommentDecorator {

  /**
   * Manage comment.
   * 
   * @param properties Properties for the API Request.
   * @param propertyComment Name of the property for the comment.
   * @param propertyTag Name of the property for the tag.
   * @param automatic True if the modification is automatic.
   */
  public void manageComment(
      Map<String, String> properties,
      String propertyComment, String propertyTag,
      boolean automatic);
}
