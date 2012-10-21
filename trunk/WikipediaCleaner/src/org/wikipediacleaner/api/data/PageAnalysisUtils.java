/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.WPCConfiguration;


/**
 * Various utilities for page analysis.
 */
public class PageAnalysisUtils {

  // ==========================================================================
  // Comments management
  // ==========================================================================

  /**
   * @param pageAnalysis Page analysis.
   * @param currentIndex Current index.
   * @return First index after comments.
   */
  public static int getFirstIndexAfterComments(
      PageAnalysis pageAnalysis, int currentIndex) {
    if (pageAnalysis == null) {
      return currentIndex;
    }
    List<PageElementComment> comments = pageAnalysis.getComments();
    for (PageElementComment comment : comments) {
      if (currentIndex < comment.getEndIndex()) {
        if (currentIndex >= comment.getBeginIndex()) {
          return comment.getEndIndex();
        }
      } else {
        return currentIndex;
      }
    }
    return currentIndex;
  }

  // ==========================================================================
  // Internal link management
  // ==========================================================================

  /**
   * Find internal links in a page.
   * 
   * @param pageAnalysis Page analysis.
   * @param links Links that are requested.
   * @param notification For notifying when a link is found.
   */
  public static void findInternalLinks(
      PageAnalysis pageAnalysis,
      List<Page> links, InternalLinkNotification notification) {
    if ((pageAnalysis == null) ||
        (links == null) ||
        (notification == null)) {
      return;
    }

    // Search for simple internal links [[link]], [[link|text]], [[link#anchor|text]], ...
    List<PageElementInternalLink> internalLinks = pageAnalysis.getInternalLinks();
    for (PageElementInternalLink internalLink : internalLinks) {
      for (Page link : links) {
        if (Page.areSameTitle(link.getTitle(), internalLink.getLink())) {
          notification.linkFound(link, internalLink);
        }
      }
    }

    // Search for internal links created by templates
    WPCConfiguration configuration = pageAnalysis.getWPCConfiguration();
    if (configuration.hasTemplateMatchers()) {
      List<PageElementTemplate> templates = pageAnalysis.getTemplates();
      for (PageElementTemplate template : templates) {
        List<? extends TemplateMatcher> matchers =
          configuration.getTemplateMatchers(template.getTemplateName());
        if (matchers != null) {
          for (TemplateMatcher matcher : matchers) {
            String linkTo = matcher.linksTo(pageAnalysis.getPage(), template);
            if (linkTo != null) {
              for (Page link : links) {
                if (Page.areSameTitle(link.getTitle(), linkTo)) {
                  notification.linkFound(link, template, matcher);
                }
              }
            }
          }
        }
      }
    }
  }

  /**
   * Get anchors in internal links.
   * 
   * @param pageAnalysis Page analysis.
   * @param pageLinks Page links.
   * @param anchors Anchors (OUT)
   */
  public static void getAnchors(PageAnalysis pageAnalysis, List<Page> pageLinks, Map<Page, List<String>> anchors) {
    if (pageAnalysis == null) {
      return;
    }
    String pageContents = pageAnalysis.getContents();
    if ((pageContents == null) ||
        (pageContents.length() == 0) ||
        (anchors == null)) {
      return;
    }

    // Check each internal link
    List<PageElementInternalLink> links = pageAnalysis.getInternalLinks();
    for (PageElementInternalLink internalLink : links) {
      String anchor = internalLink.getAnchor();
      if ((anchor != null) && (anchor.trim().length() > 0)) {
        String fullAnchor = internalLink.getFullLink();
        // Check if the internal link is for one of the links
        for (Page link : pageLinks) {
          if ((link != null) &&
              (Page.areSameTitle(link.getTitle(), internalLink.getLink()))) {
            List<String> listAnchors = anchors.get(link);
            if (listAnchors == null) {
              listAnchors = new ArrayList<String>();
              anchors.put(link, listAnchors);
            }
            if (!listAnchors.contains(fullAnchor)) {
              listAnchors.add(fullAnchor);
            }
          }
        }
      }
    }
  }

  // ==========================================================================
  // Titles management
  // ==========================================================================

  /**
   * @param position Position in the text.
   * @return All titles leading to the given position.
   */
  public static List<PageElementTitle> getCurrentTitles(
      PageAnalysis pageAnalysis, int position) {
    if (pageAnalysis == null) {
      return null;
    }

    // Analyze hierarchy of titles
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    List<PageElementTitle> currentTitles = new ArrayList<PageElementTitle>();
    for (PageElementTitle title : titles) {
      if (title.getBeginIndex() < position) {
        while ((!currentTitles.isEmpty()) &&
               (currentTitles.get(currentTitles.size() - 1).getFirstLevel() >= title.getFirstLevel())) {
          currentTitles.remove(currentTitles.size() - 1);
        }
        currentTitles.add(title);
      }
    }
    return currentTitles;
  }

  /**
   * Retrieve current chapter.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @return Current title.
   */
  public static PageElementTitle getCurrentChapter(
      PageAnalysis pageAnalysis, int position) {
    if (pageAnalysis == null) {
      return null;
    }

    // Analyze each title
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    PageElementTitle lastTitle = null;
    for (PageElementTitle title : titles) {
      if (title.getBeginIndex() < position) {
        lastTitle = title;
      } else {
        return lastTitle;
      }
    }
    return lastTitle;
  }

  /**
   * Retrieve the identifier of the current chapter.
   * 
   * @param pageAnalysis Page analysis.
   * @param position Position in the text.
   * @return Identifier of the current chapter.
   */
  public static String getCurrentChapterId(
      PageAnalysis pageAnalysis, int position) {
    if (pageAnalysis == null) {
      return null;
    }
    PageElementTitle title = getCurrentChapter(pageAnalysis, position);
    return pageAnalysis.getPage().getTitle() + "#" + ((title != null) ? title.getTitle() : "");
  }
}
