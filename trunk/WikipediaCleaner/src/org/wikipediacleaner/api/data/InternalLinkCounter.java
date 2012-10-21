/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
   */
  public void linkFound(
      Page link, PageElementInternalLink internalLink) {
    InternalLinkCount linkCount = getLinkCount(link);
    linkCount.addInternalLink();
  }

  /**
   * Notification of a link found in a template.
   * 
   * @param link Link found.
   * @param template Template in which the link is found.
   * @param matcher Matcher used to find the link in the template.
   */
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
