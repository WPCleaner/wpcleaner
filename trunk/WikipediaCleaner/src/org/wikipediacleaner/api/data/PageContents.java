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
import org.wikipediacleaner.api.constants.WPCConfiguration;


/**
 * Utility class to manage page contents.
 */
public class PageContents {

  // ==========================================================================
  // General methods
  // ==========================================================================

  /**
   * Find the first occurrence of a character in a substring.
   * 
   * @param text String.
   * @param character Character.
   * @param begin Beginning of the substring.
   * @param end End of the substring.
   * @return First occurrence of character.
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

  /**
   * Tell if an index is inside elements or not.
   * 
   * @param index Index.
   * @param elements Elements.
   * @return true if the index is inside elements.
   */
  public static boolean isInElements(
      int index, Collection<? extends PageElement> elements) {
    if (elements == null) {
      return false;
    }
    for (PageElement element : elements) {
      if ((index >= element.getBeginIndex()) &&
          (index < element.getEndIndex())) {
        return true;
      }
    }
    return false;
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
  public static List<PageElementComment> findAllComments(
      EnumWikipedia wikipedia, String contents) {
    if (contents == null) {
      return null;
    }
    List<PageElementComment> result = new ArrayList<PageElementComment>();
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
  public static List<PageElementTitle> findAllTitles(
      EnumWikipedia wikipedia, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    List<PageElementTitle> result = new ArrayList<PageElementTitle>();
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
      } else if (isInElements(tmpIndex, comments)) {
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
   * Find all internal links in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Internal links found.
   */
  public static Collection<PageElementInternalLink> findAllInternalLinks(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementInternalLink> result = new ArrayList<PageElementInternalLink>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementInternalLink link = findNextInternalLink(page, contents, currentIndex, comments);
      if (link == null) {
        currentIndex = contents.length();
      } else {
        result.add(link);
        currentIndex = link.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first internal link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Internal link found.
   */
  public static PageElementInternalLink findNextInternalLink(
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
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
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
   * @param comments Comments blocks in the page.
   * @param links Links that are requested.
   * @param notification For notifying when a link is found.
   */
  public static void findInternalLinks(
      EnumWikipedia wikipedia,
      Page page, String contents,
      Collection<PageElementComment> comments,
      List<Page> links, InternalLinkNotification notification) {
    if ((contents == null) || (links == null) || (notification == null)) {
      return;
    }

    // Search for simple internal links [[link]], [[link|text]], [[link#anchor|text]], ...
    int currentIndex = 0;
    while (currentIndex < contents.length()) {
      PageElementInternalLink internalLink = findNextInternalLink(
          page, contents, currentIndex, comments);
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
    WPCConfiguration configuration = wikipedia.getConfiguration();
    if (configuration.hasTemplateMatchers()) {
      currentIndex = 0;
      while (currentIndex < contents.length()) {
        PageElementTemplate template = PageContents.findNextTemplate(
            page, contents, currentIndex, comments);
        if (template != null) {
          currentIndex = template.getBeginIndex() + 2;
          List<? extends TemplateMatcher> matchers =
            configuration.getTemplateMatchers(template.getTemplateName());
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
   * @param comments Comments blocks in the page.
   * @param links Links that are requested.
   */
  public static void countInternalLinks(
      EnumWikipedia wikipedia,
      Page page, String contents,
      Collection<PageElementComment> comments,
      List<Page> links) {
    InternalLinkNotification counter = new InternalLinkCounter(links);
    findInternalLinks(wikipedia, page, contents, comments, links, counter);
  }

  /**
   * Count internal disambiguation links in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @param links Links that are requested.
   * @return Link count.
   */
  public static Map<String, Integer> countInternalDisambiguationLinks(
      EnumWikipedia wikipedia,
      Page page, String contents,
      Collection<PageElementComment> comments,
      List<Page> links) {
    InternalDisambiguationLinkCounter counter = new InternalDisambiguationLinkCounter();
    findInternalLinks(wikipedia, page, contents, comments, links, counter);
    return counter.getLinkCount();
  }

  // ==========================================================================
  // External link management
  // ==========================================================================

  /**
   * Find all external links in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @param templates Templates in the page.
   * @return External links found.
   */
  public static Collection<PageElementExternalLink> findAllExternalLinks(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<PageElementTemplate> templates) {
    if (contents == null) {
      return null;
    }
    List<String> protocols = PageElementExternalLink.getProtocols();
    Integer[] tmpIndexes = new Integer[protocols.size() + 1];
    Collection<PageElementExternalLink> result = new ArrayList<PageElementExternalLink>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      int tmpIndex = -1;
      for (int i = 0; i < tmpIndexes.length; i++) {
        if ((tmpIndexes[i] == null) ||
            ((tmpIndexes[i].intValue() < currentIndex) &&
             (tmpIndexes[i].intValue() >= 0))) {
          tmpIndexes[i] = Integer.valueOf((i == 0) ?
              contents.indexOf('[', currentIndex) :
              contents.indexOf(protocols.get(i - 1), currentIndex));
        }
        if (tmpIndex < 0) {
          tmpIndex = tmpIndexes[i].intValue();
        } else if ((tmpIndexes[i].intValue() < tmpIndex) && 
                   (tmpIndexes[i].intValue() >= 0)) {
          tmpIndex = tmpIndexes[i];
        }
      }
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
      } else if ((contents.charAt(tmpIndex) != '[') &&
                 (isInElements(tmpIndex, templates))) {
        currentIndex = tmpIndex + 1;
      } else {
        PageElementExternalLink link = PageElementExternalLink.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (link != null) {
          result.add(link);
          currentIndex = link.getEndIndex();
        } else {
          currentIndex = tmpIndex + 1;
        }
      }
    }
    return result;
  }

  // ==========================================================================
  // Image management
  // ==========================================================================

  /**
   * Find all images in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Images found.
   */
  public static Collection<PageElementImage> findAllImages(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementImage> result = new ArrayList<PageElementImage>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementImage image = findNextImage(page, contents, currentIndex, comments);
      if (image == null) {
        currentIndex = contents.length();
      } else {
        result.add(image);
        currentIndex = image.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first image after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Image found.
   */
  public static PageElementImage findNextImage(
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
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
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
      } else if (isInElements(tmpIndex, comments)) {
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
   * Find all interwikis in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Interwikis found.
   */
  public static Collection<PageElementInterwikiLink> findAllInterwikiLinks(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementInterwikiLink> result = new ArrayList<PageElementInterwikiLink>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementInterwikiLink link = findNextInterwikiLink(page, contents, currentIndex, comments);
      if (link == null) {
        currentIndex = contents.length();
      } else {
        result.add(link);
        currentIndex = link.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first interwiki link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Interwiki link found.
   */
  public static PageElementInterwikiLink findNextInterwikiLink(
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
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
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
   * Find all language links in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Language links found.
   */
  public static Collection<PageElementLanguageLink> findAllLanguageLinks(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementLanguageLink> result = new ArrayList<PageElementLanguageLink>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementLanguageLink link = findNextLanguageLink(page, contents, currentIndex, comments);
      if (link == null) {
        currentIndex = contents.length();
      } else {
        result.add(link);
        currentIndex = link.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first language link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Language link found.
   */
  public static PageElementLanguageLink findNextLanguageLink(
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
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
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
   * Find all DEFAULTSORT in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return DefaultSorts found.
   */
  public static Collection<PageElementDefaultsort> findAllDefaultSorts(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementDefaultsort> result = new ArrayList<PageElementDefaultsort>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementDefaultsort defaultSort = findNextDefaultsort(page, contents, currentIndex, comments);
      if (defaultSort == null) {
        currentIndex = contents.length();
      } else {
        result.add(defaultSort);
        currentIndex = defaultSort.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first DEFAULTSORT after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return DEFAULTSORT found.
   */
  public static PageElementDefaultsort findNextDefaultsort(
      Page page, String contents,
      int currentIndex,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
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
   * Find all templates in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Templates found.
   */
  public static Collection<PageElementTemplate> findAllTemplates(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    Collection<PageElementTemplate> result = new ArrayList<PageElementTemplate>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementTemplate template = findNextTemplate(page, contents, currentIndex, comments);
      if (template == null) {
        currentIndex = contents.length();
      } else {
        result.add(template);
        currentIndex = template.getBeginIndex() + 2;
      }
    }
    return result;
  }

  /**
   * Find the first template after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Template found.
   */
  public static PageElementTemplate findNextTemplate(
      Page page, String contents,
      int currentIndex,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
      } else {
        PageElementTemplate template = PageElementTemplate.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // Tag management
  // ==========================================================================

  /**
   * Find all tags in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments blocks in the page.
   * @return Tags found.
   */
  public static List<PageElementTag> findAllTags(
      Page page, String contents,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    List<PageElementTag> result = new ArrayList<PageElementTag>();
    int currentIndex = 0;
    while ((currentIndex < contents.length())) {
      PageElementTag tag = findNextTag(page, contents, currentIndex, comments);
      if (tag == null) {
        currentIndex = contents.length();
      } else {
        if (tag.isEndTag() && !tag.isFullTag()) {
          boolean found = false;
          int i = result.size();
          int level = 0;
          while ((i > 0) && !found) {
            i--;
            PageElementTag tmpTag = result.get(i);
            if (tag.getNormalizedName().equals(tmpTag.getNormalizedName())) {
              if (!tmpTag.isFullTag()) {
                if (tmpTag.isEndTag()) {
                  level++;
                } else {
                  level--;
                  if (level < 0) {
                    found = true;
                    tmpTag.setMatchingTag(tag);
                  }
                }
              }
            }
          }
        }
        result.add(tag);
        currentIndex = tag.getEndIndex();
      }
    }
    return result;
  }

  /**
   * Find the first tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @param comments Comments blocks in the page.
   * @return Tag found.
   */
  public static PageElementTag findNextTag(
      Page page, String contents,
      int currentIndex,
      Collection<PageElementComment> comments) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf('<', currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else if (isInElements(tmpIndex, comments)) {
        currentIndex = indexAfterComments(tmpIndex, comments);
      } else {
        PageElementTag tag = PageElementTag.analyzeBlock(
            contents, tmpIndex);
        if (tag != null) {
          return tag;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  /**
   * Find the first tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tagName Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTagFull findNextTagFull(
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
        PageElementTagFull tag = PageElementTagFull.analyzeBlock(
            tagName, contents, tmpIndex);
        if (tag != null) {
          return tag;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }
}
