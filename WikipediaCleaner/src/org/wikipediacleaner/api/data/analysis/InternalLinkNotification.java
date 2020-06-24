/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.analysis;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateMatcher;

/**
 * An interface for notification when internal links are found.
 */
public interface InternalLinkNotification {

  /**
   * Notification of a link found in an internal link.
   * 
   * @param link Link found.
   * @param internalLink Internal link in which the link is found.
   * @param good True if link is good.
   * @param helpNeeded True if help is needed.
   */
  public void linkFound(
      Page link, PageElementInternalLink internalLink,
      boolean good, boolean helpNeeded);

  /**
   * Notification of a link found in a template.
   * 
   * @param link Link found.
   * @param template Template in which the link is found.
   * @param matcher Matcher used to find the link in the template.
   */
  public void linkFound(
      Page link, PageElementTemplate template, TemplateMatcher matcher);
}
