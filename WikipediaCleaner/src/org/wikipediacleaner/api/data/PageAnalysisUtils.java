/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.InternalLinkNotification;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsComment;


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
    List<ContentsComment> comments = pageAnalysis.getComments();
    for (ContentsComment comment : comments) {
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
    WPCConfiguration wpcConfiguration = pageAnalysis.getWPCConfiguration();
    List<String> templatesAfter = wpcConfiguration.getStringList(
        WPCConfigurationStringList.TEMPLATES_AFTER_HELP_ASKED);
    List<String> commentsAfter = wpcConfiguration.getStringList(
        WPCConfigurationStringList.COMMENTS_FOR_DAB_LINK);
    List<String[]> templatesIgnoreDab = wpcConfiguration.getStringArrayList(
        WPCConfigurationStringList.TEMPLATES_IGNORE_DAB);
    MagicWord redirect = pageAnalysis.getWikiConfiguration().getMagicWordByName(MagicWord.REDIRECT);
    String contents = pageAnalysis.getContents();
    int maxSize = contents.length();
    boolean firstLink = true;
    for (PageElementInternalLink internalLink : internalLinks) {
      for (Page link : links) {
        if (Page.areSameTitle(link.getTitle(), internalLink.getLink())) {
          int currentPos = internalLink.getEndIndex();
          while ((currentPos < maxSize) && (contents.charAt(currentPos) == ' ')) {
            currentPos++;
          }

          // Check if link is marked as needing help
          boolean helpNeeded = false;
          if (templatesAfter != null) {
            if ((currentPos < maxSize) && (contents.charAt(currentPos) == '{')) {
              PageElementTemplate nextTemplate = pageAnalysis.isInTemplate(currentPos);
              if (nextTemplate != null) {
                for (String templateAfter : templatesAfter) {
                  if (Page.areSameTitle(templateAfter, nextTemplate.getTemplateName())) {
                    helpNeeded = true;
                  }
                }
              }
            }
          }

          // Check if link is marked as normal
          boolean good = false;
          if ((currentPos < maxSize) && (contents.charAt(currentPos) == '<')) {
            ContentsComment nextComment = pageAnalysis.isInComment(currentPos);
            if ((nextComment != null) && (nextComment.getComment() != null)) {
              if (commentsAfter != null) {
                for (String commentAfter : commentsAfter) {
                  if (nextComment.getComment().length() >= commentAfter.length()) {
                    String comment = nextComment.getComment().substring(0, commentAfter.length());
                    if (comment.equalsIgnoreCase(commentAfter)) {
                      good = true;
                    }
                  }
                }
              }
            }
          }
          if (!good &&
              (templatesIgnoreDab != null) &&
              !templatesIgnoreDab.isEmpty()) {
            PageElementTemplate template = pageAnalysis.isInTemplate(currentPos);
            if (template != null) {
              for (String[] currentTemplate : templatesIgnoreDab) {
                if ((currentTemplate != null) &&
                    (currentTemplate.length > 1) &&
                    Page.areSameTitle(currentTemplate[0], template.getTemplateName())) {
                  Parameter parameter = template.getParameterAtIndex(currentPos);
                  if (parameter != null) {
                    for (int index = 1; index < currentTemplate.length; index++) {
                      if (parameter.getComputedName().equals(currentTemplate[index])) {
                        good = true;
                      }
                    }
                  }
                }
              }
            }
          }

          // Check if link is in fact a redirection
          if (firstLink && (redirect != null)) {
            int tmpPos = 0;
            while ((contents.charAt(tmpPos) == ' ') &&
                   (tmpPos < internalLink.getBeginIndex())) {
              tmpPos++;
            }
            String redirectTag = null;
            for (String alias : redirect.getAliases()) {
              if (contents.startsWith(alias, tmpPos)) {
                char next = contents.charAt(tmpPos + alias.length());
                if ((next == ' ') || (next == '[')) {
                  redirectTag = alias;
                }
              }
            }
            if (redirectTag != null) {
              tmpPos += redirectTag.length();
              while (contents.charAt(tmpPos) == ' ') {
                tmpPos++;
              }
              if (tmpPos == internalLink.getBeginIndex()) {
                good = true;
              }
            }
          }

          notification.linkFound(link, internalLink, good, helpNeeded);
        }
      }
      firstLink = false;
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
   * @param pageAnalysis Page analysis.
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
               (currentTitles.get(currentTitles.size() - 1).getLevel() >= title.getLevel())) {
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
