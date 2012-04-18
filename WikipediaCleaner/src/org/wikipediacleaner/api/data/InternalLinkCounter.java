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


/**
 * A notification class to count links in a page.
 */
public class InternalLinkCounter implements InternalLinkNotification {

  /**
   * Constructor.
   */
  public InternalLinkCounter(List<Page> links) {
    if (links != null) {
      for (Page link : links) {
        link.setCountOccurrence(0);
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
    countLink(link);
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
    countLink(link);
  }

  /**
   * Increment the number of times a link has been found.
   * 
   * @param link Link found.
   */
  private void countLink(Page link) {
    if (link == null) {
      return;
    }
    link.setCountOccurrence(link.getCountOccurrence() + 1);
  }
}
