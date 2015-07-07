/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;
import java.util.Map;


/**
 * A notification class to count links in a page.
 */
public class InternalLinkCounter implements InternalLinkNotification {

  private final Map<String, InternalLinkCount> linksCount;

  /**
   * Constructor.
   */
  InternalLinkCounter(Map<String, InternalLinkCount> linksCount, List<Page> links) {
    this.linksCount = linksCount;
    if (links != null) {
      for (Page link : links) {
        getLinkCount(link);
      }
    }
  }

  /**
   * Notification of a link found in an internal link.
   * 
   * @param link Link found.
   * @param internalLink Internal link in which the link is found.
   * @param good True if link is good.
   * @param helpNeeded True if help is needed.
   */
  @Override
  public void linkFound(
      Page link, PageElementInternalLink internalLink,
      boolean good, boolean helpNeeded) {
    InternalLinkCount linkCount = getLinkCount(link);
    if (good) {
      linkCount.addGoodInternalLink();
    } else if (helpNeeded) {
      linkCount.addHelpNeededInternalLink();
    } else {
      linkCount.addInternalLink();
    }
  }

  /**
   * Notification of a link found in a template.
   * 
   * @param link Link found.
   * @param template Template in which the link is found.
   * @param matcher Matcher used to find the link in the template.
   */
  @Override
  public void linkFound(
      Page link, PageElementTemplate template,
      TemplateMatcher matcher) {
    InternalLinkCount linkCount = getLinkCount(link);
    if (matcher.isHelpNeeded()) {
      linkCount.addHelpNeededTemplateLink();
    } else if (matcher.isGood()) {
      linkCount.addGoodTemplateLink();
    } else {
      linkCount.addIncorrectTemplateLink();
    }
  }

  /**
   * @param link Link requested.
   * @return Structure for counting links.
   */
  private InternalLinkCount getLinkCount(Page link) {
    String title = link.getTitle();
    InternalLinkCount linkCount = linksCount.get(title);
    if (linkCount == null) {
      linkCount = new InternalLinkCount(title);
      linksCount.put(title, linkCount);
    }
    return linkCount;
  }
}
