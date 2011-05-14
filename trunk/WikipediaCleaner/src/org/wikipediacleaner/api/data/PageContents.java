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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Utility class to manage page contents.
 */
public class PageContents {

  // ==========================================================================
  // General methods
  // ==========================================================================

  /**
   * Find the first occurence of a character in a substring.
   * 
   * @param text String.
   * @param character Character.
   * @param begin Beginning of the substring.
   * @param end End of the substring.
   * @return First occurence of character.
   */
  public static int findCharacter(
      String text, char character, int begin, int end) {
    if (text == null) {
      return -1;
    }
    for (int i = begin; i < end; i++) {
      if (text.charAt(i) == character) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Expand text for a page (for example, replacing {{PAGENAME}}).
   * 
   * @param page Page.
   * @param text Text to expand.
   * @return Expanded text.
   */
  public static String expandText(Page page, String text) {
    if ((page == null) || (text == null)) {
      return text;
    }
    String result = text;
    result = result.replaceAll("\\{\\{PAGENAME\\}\\}", page.getValuePAGENAME());
    return result;
  }

  // ==========================================================================
  // Comments management
  // ==========================================================================

  /**
   * Find all comments in the page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Comments found.
   */
  public static Collection<PageElementComment> findAllComments(
      EnumWikipedia wikipedia, String contents) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementComment> result = new ArrayList<PageElementComment>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementComment comment = findNextComment(wikipedia, contents, currentIndex);
      if (comment == null) {
        currentIndex = contents.length();
      } else {
        result.add(comment);
        currentIndex = comment.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first comment after an index in the page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Comment found.
   */
  public static PageElementComment findNextComment(
      EnumWikipedia wikipedia, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("<!--", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementComment comment = PageElementComment.analyzeBlock(
            wikipedia, contents, tmpIndex);
        if (comment != null) {
          return comment;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  /**
   * Tell if an index is inside comments or not.
   * 
   * @param index Index.
   * @param comments Comments.
   * @return true if the index is inside comments.
   */
  public static boolean isInComments(
      int index, Collection<PageElementComment> comments) {
    if (comments == null) {
      return false;
    }
    for (PageElementComment comment : comments) {
      if ((index >= comment.getBeginIndex()) &&
          (index < comment.getEndIndex())) {
        return true;
      }
    }
    return false;
  }

  /**
   * @param index Index.
   * @param comments Comments.
   * @return First index after comments.
   */
  public static int indexAfterComments(
      int index, Collection<PageElementComment> comments) {
    if (comments == null) {
      return index;
    }
    for (PageElementComment comment : comments) {
      if ((index >= comment.getBeginIndex()) &&
          (index < comment.getEndIndex())) {
        return comment.getEndIndex();
      }
    }
    return index;
  }

  // ==========================================================================
  // Table of Contents management
  // ==========================================================================

  /**
   * Find all titles in the page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Titles found.
   */
  public static Collection<PageElementTitle> findAllTitles(
      EnumWikipedia wikipedia, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementTitle> result = new ArrayList<PageElementTitle>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementTitle title = findNextTitle(wikipedia, contents, currentIndex, comments);
      if (title == null) {
        currentIndex = contents.length();
      } else {
        result.add(title);
        currentIndex = title.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first title after an index in the page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Page contents.
   * @param currentIndex Current index.
   * @param comments Comments blocks in the page.
   * @return Title found.
   */
  public static PageElementTitle findNextTitle(
      EnumWikipedia wikipedia, String contents,
      int currentIndex,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("=", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else if (isInComments(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
      } else {
        PageElementTitle title = PageElementTitle.analyzeBlock(
            wikipedia, contents, tmpIndex);
        if (title != null) {
          return title;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  /**
   * Retrieve current chapter hierarchy.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Page contents (may be different from page.getContents()).
   * @param position Position in the text.
   * @param comments Comments blocks in the page.
   * @return Chapters.
   */
  public static List<PageElementTitle> getChapterPosition(
      EnumWikipedia wikipedia, String contents,
      int position,
      Collection<PageElementComment> comments) {

    if (contents == null) {
      return null;
    }

    // Analyze text for titles
    List<PageElementTitle> chapters = new ArrayList<PageElementTitle>();
    int startIndex = 0;
    while ((startIndex < position) && (startIndex < contents.length())) {
      PageElementTitle title = findNextTitle(wikipedia, contents, startIndex, comments);
      if (title == null) {
        startIndex = contents.length();
      } else {
        if (title.getBeginIndex() < position) {
          while (!chapters.isEmpty() &&
                 (chapters.get(chapters.size() - 1).getFirstLevel() >= title.getFirstLevel())) {
            chapters.remove(chapters.size() - 1);
          }
          chapters.add(title);
        }
        startIndex = title.getEndIndex();
      }
    }
    return chapters;
  }

  // ==========================================================================
  // Internal link management
  // ==========================================================================

  /**
   * Find the first internal link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Internal link found.
   */
  public static PageElementInternalLink findNextInternalLink(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("[[", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementInternalLink link = PageElementInternalLink.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (link != null) {
          return link;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  /**
   * Find internal links in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param links Links that are requested.
   * @param notification For notifying when a link is found.
   */
  public static void findInternalLinks(
      EnumWikipedia wikipedia,
      Page page, String contents,
      List<Page> links, InternalLinkNotification notification) {
    if ((contents == null) || (links == null) || (notification == null)) {
      return;
    }

    // Search for simple internal links [[link]], [[link|text]], [[link#anchor|text]], ...
    int currentIndex = 0;
    while (currentIndex < contents.length()) {
      PageElementInternalLink internalLink = findNextInternalLink(page, contents, currentIndex);
      if (internalLink != null) {
        currentIndex = internalLink.getBeginIndex() + 2;
        for (Page link : links) {
          if (Page.areSameTitle(link.getTitle(), internalLink.getLink())) {
            notification.linkFound(link, internalLink);
          }
        }
      } else {
        currentIndex = contents.length();
      }
    }

    // Search for internal links created by templates
    if (wikipedia.hasTemplateMatchers()) {
      currentIndex = 0;
      while (currentIndex < contents.length()) {
        PageElementTemplate template = PageContents.findNextTemplate(page, contents, currentIndex);
        if (template != null) {
          currentIndex = template.getBeginIndex() + 2;
          List<? extends TemplateMatcher> matchers =
            wikipedia.getTemplateMatchers(template.getTemplateName());
          if (matchers != null) {
            for (TemplateMatcher matcher : matchers) {
              String linkTo = matcher.linksTo(page, template);
              if (linkTo != null) {
                for (Page link : links) {
                  if (Page.areSameTitle(link.getTitle(), linkTo)) {
                    notification.linkFound(link, template, matcher);
                  }
                }
              }
            }
          }
        } else {
          currentIndex = contents.length();
        }
      }
    }
  }

  /**
   * Count internal links in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param links Links that are requested.
   */
  public static void countInternalLinks(
      EnumWikipedia wikipedia,
      Page page, String contents,
      List<Page> links) {
    InternalLinkNotification counter = new InternalLinkCounter(links);
    findInternalLinks(wikipedia, page, contents, links, counter);
  }

  /**
   * Count internal disambiguation links in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param links Links that are requested.
   * @return Link count.
   */
  public static Map<String, Integer> countInternalDisambiguationLinks(
      EnumWikipedia wikipedia,
      Page page, String contents,
      List<Page> links) {
    InternalDisambiguationLinkCounter counter = new InternalDisambiguationLinkCounter();
    findInternalLinks(wikipedia, page, contents, links, counter);
    return counter.getLinkCount();
  }

  // ==========================================================================
  // Image management
  // ==========================================================================

  /**
   * Find the first image after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Image found.
   */
  public static PageElementImage findNextImage(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("[[", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementImage image = PageElementImage.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (image != null) {
          return image;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // Category management
  // ==========================================================================

  /**
   * Find all categories in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Categories found.
   */
  public static Collection<PageElementCategory> findAllCategories(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementCategory> result = new ArrayList<PageElementCategory>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementCategory category = findNextCategory(page, contents, currentIndex, comments);
      if (category == null) {
        currentIndex = contents.length();
      } else {
        result.add(category);
        currentIndex = category.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first category after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Category found.
   */
  public static PageElementCategory findNextCategory(
      Page page, String contents,
      int currentIndex,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("[[", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else if (isInComments(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
      } else {
        PageElementCategory category = PageElementCategory.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (category != null) {
          return category;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // Interwiki links management
  // ==========================================================================

  /**
   * Find the first interwiki link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Interwiki link found.
   */
  public static PageElementInterwikiLink findNextInterwikiLink(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("[[", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementInterwikiLink link = PageElementInterwikiLink.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (link != null) {
          return link;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // Language links management
  // ==========================================================================

  /**
   * Find the first language link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Language link found.
   */
  public static PageElementLanguageLink findNextLanguageLink(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("[[", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementLanguageLink link = PageElementLanguageLink.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (link != null) {
          return link;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // DEFAULTSORT management
  // ==========================================================================

  /**
   * Find the first DEFAULTSORT after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return DEFAULTSORT found.
   */
  public static PageElementDefaultsort findNextDefaultsort(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementDefaultsort template = PageElementDefaultsort.analyzeBlock(page.getWikipedia(), contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // Template management
  // ==========================================================================

  /**
   * Find the first template after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTemplate findNextTemplate(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementTemplate template = PageElementTemplate.analyzeBlock(null, contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  /**
   * Find the first template after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param templateName Template to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTemplate findNextTemplate(
      Page page, String contents,
      String templateName, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementTemplate template = PageElementTemplate.analyzeBlock(templateName, contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  // ==========================================================================
  // Tag management
  // ==========================================================================

  /**
   * Find the first tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tagName Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTag findNextTag(
      Page page, String contents,
      String tagName, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementTag tag = PageElementTag.analyzeBlock(
            tagName, contents, tmpIndex);
        if (tag != null) {
          return tag;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  /**
   * Find the last start tag before an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTagData findPreviousStartTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex > 0) {
      int tmpIndex = contents.lastIndexOf(">", currentIndex - 1);
      if (tmpIndex < 0) {
        currentIndex = 0;
      } else {
        int infoTagEnd = tmpIndex + 1;
        tmpIndex--;
        currentIndex = tmpIndex;
        // Possible whitespaces
        while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex--;
        }
        int startIndex = contents.lastIndexOf("<", tmpIndex);
        if (startIndex < 0) {
          currentIndex = 0;
        } else {
          int previousEndIndex = contents.lastIndexOf(">", tmpIndex);
          if (previousEndIndex < startIndex) {
            int infoTagStart = startIndex;
            startIndex++;
            // Possible whitespaces
            while ((startIndex < tmpIndex) && (contents.charAt(startIndex) == ' ')) {
              startIndex++;
            }
            if (tag.equalsIgnoreCase(contents.substring(startIndex, startIndex + tag.length()))) {
              startIndex += tag.length();
              if ((contents.charAt(startIndex) == ' ') ||
                  (contents.charAt(startIndex) == '>')) {
                return new PageElementTagData(infoTagStart, infoTagEnd); 
              }
            }
          }
        }
      }
    }
    return null;
  }

  /**
   * Find the first start tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTagData findNextStartTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        int infoTagStart = tmpIndex;
        tmpIndex++;
        currentIndex = tmpIndex;
        // Possible whitespaces
        while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        int endIndex = contents.indexOf(">", tmpIndex);
        if (endIndex < 0) {
          currentIndex = contents.length();
        } else {
          int nextStartIndex = contents.indexOf("<", tmpIndex);
          if ((nextStartIndex < 0) || (endIndex < nextStartIndex)) {
            int infoTagEnd = endIndex + 1;
            if (tag.equalsIgnoreCase(contents.substring(tmpIndex, tmpIndex + tag.length()))) {
              tmpIndex += tag.length();
              if ((contents.charAt(tmpIndex) == ' ') ||
                  (contents.charAt(tmpIndex) == '>')) {
                return new PageElementTagData(infoTagStart, infoTagEnd); 
              }
            }
          }
        }
      }
    }
    return null;
  }

  /**
   * Find the last end tag before an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTagData findPreviousEndTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex > 0) {
      int tmpIndex = contents.lastIndexOf(">", currentIndex - 1);
      if (tmpIndex < 0) {
        currentIndex = 0;
      } else {
        int infoTagEnd = tmpIndex + 1;
        tmpIndex--;
        currentIndex = tmpIndex;
        // Possible whitespaces
        while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex--;
        }
        int startIndex = contents.lastIndexOf("<", tmpIndex);
        if (startIndex < 0) {
          currentIndex = 0;
        } else {
          int previousEndIndex = contents.lastIndexOf(">", tmpIndex);
          if (previousEndIndex < startIndex) {
            int infoTagStart = startIndex;
            startIndex++;
            if ((startIndex < tmpIndex) && (contents.charAt(startIndex) == '/')) {
              startIndex++;
              // Possible whitespaces
              while ((startIndex < tmpIndex) && (contents.charAt(startIndex) == ' ')) {
                startIndex++;
              }
              if (tag.equalsIgnoreCase(contents.substring(startIndex, startIndex + tag.length()))) {
                startIndex += tag.length();
                if ((contents.charAt(startIndex) == ' ') ||
                    (contents.charAt(startIndex) == '>')) {
                  return new PageElementTagData(infoTagStart, infoTagEnd); 
                }
              }
            }
          }
        }
      }
    }
    return null;
  }

  /**
   * Find the first end tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTagData findNextEndTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        int infoTagStart = tmpIndex;
        tmpIndex++;
        currentIndex = tmpIndex;
        if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '/')) {
          tmpIndex++;
          // Possible whitespaces
          while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
            tmpIndex++;
          }
          int endIndex = contents.indexOf(">", tmpIndex);
          if (endIndex < 0) {
            currentIndex = contents.length();
          } else {
            int nextStartIndex = contents.indexOf("<", tmpIndex);
            if ((nextStartIndex < 0) || (endIndex < nextStartIndex)) {
              int infoTagEnd = endIndex + 1;
              if ((tmpIndex + tag.length() <= contents.length()) &&
                  (tag.equalsIgnoreCase(contents.substring(tmpIndex, tmpIndex + tag.length())))) {
                tmpIndex += tag.length();
                if ((contents.charAt(tmpIndex) == ' ') ||
                    (contents.charAt(tmpIndex) == '>')) {
                  return new PageElementTagData(infoTagStart, infoTagEnd); 
                }
              }
            }
          }
        }
      }
    }
    return null;
  }
}
