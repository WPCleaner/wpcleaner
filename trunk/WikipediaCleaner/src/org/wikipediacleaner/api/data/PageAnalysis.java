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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * An analysis of a page.
 */
public class PageAnalysis {

  /**
   * Page currently analyzed.
   */
  private final Page page;

  /**
   * Current version of the text.
   */
  private final String contents;

  /**
   * True if spelling should be checked.
   */
  private boolean checkSpelling;

  /**
   * @param page Page.
   * @param contents Page contents (may differ from page.getContents()).
   */
  PageAnalysis(Page page, String contents) {
    this.page = page;
    this.contents = (contents != null) ? contents : page.getContents();
    this.areas = new PageElementAreas();

    // Default configuration
    Configuration config = Configuration.getConfiguration();
    checkSpelling = config.getBoolean(
        null, ConfigurationValueBoolean.SPELLING);
  }

  /**
   * @return Page.
   */
  public Page getPage() {
    return page;
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    if (page != null) {
      return page.getWikipedia();
    }
    return null;
  }

  /**
   * @return Wiki settings.
   */
  public AbstractWikiSettings getSettings() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      return wikipedia.getSettings();
    }
    return null;
  }

  /**
   * @return Wiki configuration.
   */
  public WikiConfiguration getWikiConfiguration() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      return wikipedia.getWikiConfiguration();
    }
    return null;
  }

  /**
   * @return WPCleaner configuration.
   */
  public WPCConfiguration getWPCConfiguration() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      return wikipedia.getConfiguration();
    }
    return null;
  }

  /**
   * @param namespace Namespace.
   * @return true if the page is in the namespace.
   */
  public boolean isInNamespace(int namespace) {
    if ((page != null) &&
        (page.getNamespace() != null)) {
      return (page.getNamespace().intValue() == namespace);
    }
    return false;
  }

  /**
   * @return Page contents.
   */
  public String getContents() {
    return contents;
  }

  /**
   * @param check True if spelling should be checked.
   */
  public void shouldCheckSpelling(boolean check) {
    this.checkSpelling = check;
  }

  /**
   * @return True if spelling should be checked.
   */
  public boolean shouldCheckSpelling() {
    return checkSpelling;
  }

  /**
   * Perform page analysis.
   */
  public void performFullPageAnalysis() {
    firstLevelAnalysis();
    secondLevelAnalysis();
    thirdLevelAnalysis();
  }

  // ==========================================================================
  // Elements management
  // ==========================================================================

  /**
   * Management of non wiki text areas.
   */
  private final PageElementAreas areas;

  /**
   * @return List of non wiki text areas.
   */
  public PageElementAreas getAreas() {
    thirdLevelAnalysis();
    return areas;
  }

  public List<PageElement> getElements(
      boolean withCategories, boolean withComments,
      boolean withDefaultsorts, boolean withExternalLinks,
      boolean withImages, boolean withInternalLinks,
      boolean withInterwikiLinks, boolean withLanguageLinks,
      boolean withTags, boolean withTemplates, boolean withTitles) {
    List<PageElement> elements = new ArrayList<PageElement>();
    if (withCategories) {
      elements.addAll(getCategories());
    }
    if (withComments) {
      elements.addAll(getComments());
    }
    if (withDefaultsorts) {
      elements.addAll(getDefaultSorts());
    }
    if (withExternalLinks) {
      elements.addAll(getExternalLinks());
    }
    if (withImages) {
      elements.addAll(getImages());
    }
    if (withInternalLinks) {
      elements.addAll(getInternalLinks());
    }
    if (withInterwikiLinks) {
      elements.addAll(getInterwikiLinks());
    }
    if (withLanguageLinks) {
      elements.addAll(getLanguageLinks());
    }
    if (withTags) {
      elements.addAll(getTags());
    }
    if (withTemplates) {
      elements.addAll(getTemplates());
    }
    if (withTitles) {
      elements.addAll(getTitles());
    }
    Collections.sort(elements, new PageElementComparator());
    return elements;
  }

  /**
   * @param currentIndex Index.
   * @return Element at the specified index.
   */
  public PageElement isInElement(int currentIndex) {

    // Check if in comment
    PageElement element = isInComment(currentIndex);
    if (element != null) {
      return element;
    }

    // Check if in internal link
    PageElementInternalLink internalLink = isInInternalLink(currentIndex);
    element = internalLink;

    // Check if in template
    PageElementTemplate template = isInTemplate(currentIndex);
    if ((template != null) &&
        ((element == null) || (element.getBeginIndex() < template.getBeginIndex()))) {
      element = template;
    }

    // Check if in image
    PageElementImage image = isInImage(currentIndex);
    if ((image != null) &&
        ((element == null) || (element.getBeginIndex() < image.getBeginIndex()))) {
      element = image;
    }

    // Check if in category
    PageElementCategory category = isInCategory(currentIndex);
    if ((category != null) &&
        ((element == null) || (element.getBeginIndex() < category.getBeginIndex()))) {
      element = category;
    }

    // Check if in interwiki
    PageElementInterwikiLink interwiki = isInInterwikiLink(currentIndex);
    if ((interwiki != null) &&
        ((element == null) || (element.getBeginIndex() < interwiki.getBeginIndex()))) {
      element = interwiki;
    }

    // Check if in language link
    PageElementLanguageLink language = isInLanguageLink(currentIndex);
    if ((language != null) &&
        ((element == null) || (element.getBeginIndex() < language.getBeginIndex()))) {
      element = language;
    }

    // Check if in external link
    PageElementExternalLink externalLink = isInExternalLink(currentIndex);
    if ((externalLink != null) &&
        ((element == null) || (element.getBeginIndex() < externalLink.getBeginIndex()))) {
      element = externalLink;
    }

    // Check if in tag
    PageElementTag tag = isInTag(currentIndex);
    if ((tag != null) &&
        ((element == null) || (element.getBeginIndex() < tag.getBeginIndex()))) {
      element = tag;
    }

    return element;
  }

  // ==========================================================================
  // Content analysis
  // ==========================================================================

  /**
   * Internal lock for first level analysis.
   */
  private final Object firstLevelLock = new Object();

  /**
   * Internal lock for second level analysis.
   */
  private final Object secondLevelLock = new Object();

  /**
   * Internal lock for third level analysis.
   */
  private final Object thirdLevelLock = new Object();

  /**
   * Perform a first level analysis of the page (comments).
   */
  private void firstLevelAnalysis() {
    synchronized (firstLevelLock) {
      if (comments != null) {
        return;
      }

      // Initialize
      comments = new ArrayList<PageElementComment>();

      // Go through all the text of the page
      int maxIndex = contents.length();
      int currentIndex = 0;
      while (currentIndex < maxIndex) {
        currentIndex = contents.indexOf("<!--", currentIndex);
        if (currentIndex < 0) {
          currentIndex = maxIndex;
        } else {
          PageElementComment comment = PageElementComment.analyzeBlock(
              getWikipedia(), contents, currentIndex);
          if (comment != null) {
            comments.add(comment);
            currentIndex = comment.getEndIndex();
          } else {
            currentIndex++;
          }
        }
      }

      // Update areas of non wiki text
      areas.addComments(comments);
    }
  }

  /**
   * Perform a second level analysis of the page (tags).
   */
  private void secondLevelAnalysis() {
    synchronized (secondLevelLock) {
      if (tags != null) {
        return;
      }
      firstLevelAnalysis();

      // Initialize
      tags = new ArrayList<PageElementTag>();

      // Go through all the text of the page
      int maxIndex = contents.length();
      int currentIndex = 0;
      while (currentIndex < maxIndex) {
        currentIndex = contents.indexOf('<', currentIndex);
        if (currentIndex < 0) {
          currentIndex = maxIndex;
        } else {
          int nextIndex = areas.getEndArea(currentIndex);
          if (nextIndex > currentIndex) {
            currentIndex = nextIndex;
          } else {
            PageElementTag tag = PageElementTag.analyzeBlock(contents, currentIndex);
            if (tag != null) {
              if (tag.isEndTag() && !tag.isFullTag()) {
                boolean found = false;
                int i = tags.size();
                int level = 0;
                while ((i > 0) && !found) {
                  i--;
                  PageElementTag tmpTag = tags.get(i);
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
              tags.add(tag);
              currentIndex = tag.getEndIndex();
            } else {
              currentIndex++;
            }
          }
        }
      }

      // Update areas of non wiki text
      areas.addTags(tags);
    }
  }

  /**
   * Perform a third level analysis of the page (links, templates, ...).
   */
  private void thirdLevelAnalysis() {
    synchronized (thirdLevelLock) {
      if (internalLinks != null) {
        return;
      }
      secondLevelAnalysis();
      internalLinks = new ArrayList<PageElementInternalLink>();
      images = new ArrayList<PageElementImage>();
      categories = new ArrayList<PageElementCategory>();
      interwikiLinks = new ArrayList<PageElementInterwikiLink>();
      languageLinks = new ArrayList<PageElementLanguageLink>();
      templates = new ArrayList<PageElementTemplate>();
      defaultSorts = new ArrayList<PageElementDefaultsort>();
      titles = new ArrayList<PageElementTitle>();
      externalLinks = new ArrayList<PageElementExternalLink>();

      // Go through all the text of the page
      int maxIndex = contents.length();
      int currentIndex = 0;
      while (currentIndex < maxIndex) {

        // Checking if the current index is in wiki text area.
        int nextIndex = areas.getEndArea(currentIndex);
        if (nextIndex > currentIndex) {
          currentIndex = nextIndex;
        } else {
          if (contents.startsWith("[[", currentIndex)) {
            currentIndex = analyze2SquareBrackets(currentIndex);
          } else if (contents.startsWith("[", currentIndex)) {
            currentIndex = analyze1SquareBracket(currentIndex);
          } else if (contents.startsWith("{{", currentIndex)) {
            currentIndex = analyze2CurlyBrackets(currentIndex);
          } else if (contents.startsWith("=", currentIndex)) {
            currentIndex = analyze1Equal(currentIndex);
          } else {
            currentIndex = analyzeText(currentIndex);
          }
        }
      }

      // Update areas of non wiki text
      areas.addInternalLinks(internalLinks);
      areas.addImages(images);
      areas.addCategories(categories);
      areas.addInterwikiLinks(interwikiLinks);
      areas.addLanguageLinks(languageLinks);
      areas.addTemplates(templates);
      areas.addDefaultSorts(defaultSorts);
      areas.addTitles(titles);
      areas.addExternalLinks(externalLinks);
    }
  }

  /**
   * Part of the third level of analysis when text is beginning with "[[".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze2SquareBrackets(int currentIndex) {

    // Check if this is an internal link
    PageElementInternalLink link = PageElementInternalLink.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (link != null) {
      internalLinks.add(link);
      if (link.getText() == null) {
        return link.getEndIndex();
      }
      return link.getBeginIndex() + Math.max(2, link.getTextOffset());
    }

    // Check if this is an image
    PageElementImage image = PageElementImage.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (image != null) {
      images.add(image);
      return image.getBeginIndex() + image.getFirstPipeOffset();
    }

    // Check if this is a category
    PageElementCategory category = PageElementCategory.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (category != null) {
      categories.add(category);
      return category.getEndIndex();
    }

    // Check if this is an interwiki link
    PageElementInterwikiLink interwiki = PageElementInterwikiLink.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (interwiki != null) {
      interwikiLinks.add(interwiki);
      if (interwiki.getText() == null) {
        return interwiki.getEndIndex();
      }
      return interwiki.getBeginIndex() + Math.max(2, interwiki.getTextOffset());
    }

    // Check if this is a language link
    PageElementLanguageLink language = PageElementLanguageLink.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (language != null) {
      languageLinks.add(language);
      return language.getEndIndex();
    }

    return currentIndex + 1;
  }

  /**
   * Part of the third level of analysis when text is beginning with "[".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze1SquareBracket(int currentIndex) {

    // Check if this an external link
    PageElementExternalLink link = PageElementExternalLink.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (link != null) {
      externalLinks.add(link);
      if (link.getText() == null) {
        return link.getEndIndex();
      }
      return link.getBeginIndex() + Math.max(2, link.getTextOffset());
    }

    return currentIndex + 1;
  }

  /**
   * Part of the third level of analysis for regular text.
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyzeText(int currentIndex) {

    // Check if this is an external link
    if ((externalLinks.size() == 0) ||
        (externalLinks.get(externalLinks.size() - 1).getEndIndex() <= currentIndex)) {
      PageElementExternalLink link = PageElementExternalLink.analyzeBlock(
          getWikipedia(), contents, currentIndex);
      if (link != null) {
        externalLinks.add(link);
        return link.getEndIndex();
      }
    }

    return currentIndex + 1;
  }

  /**
   * Part of the third level of analysis when text is beginning with "{{".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze2CurlyBrackets(int currentIndex) {

    // Check if this is a template
    PageElementTemplate template = PageElementTemplate.analyzeBlock(
        getWikipedia(), contents, currentIndex,
        comments, tags);
    if (template != null) {
      templates.add(template);
      return currentIndex + 2;
    }

    // Check if this is a DEFAULTSORT
    PageElementDefaultsort defaultSort = PageElementDefaultsort.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (defaultSort != null) {
      defaultSorts.add(defaultSort);
      return defaultSort.getEndIndex();
    }

    return currentIndex + 1;
  }

  /**
   * Part of the third level of analysis when text is beginning with "=".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze1Equal(int currentIndex) {

    // Check if this is a title
    PageElementTitle title = PageElementTitle.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (title != null) {
      titles.add(title);
      return title.getBeginIndex() + title.getFirstLevel();
    }

    return currentIndex + 1;
  }

  // ==========================================================================
  // Comments management
  // ==========================================================================

  /**
   * All comments in the page
   */
  private List<PageElementComment> comments;

  /**
   * @return All comments in the page.
   */
  public List<PageElementComment> getComments() {
    firstLevelAnalysis();
    return comments;
  }

  /**
   * @param currentIndex Current index.
   * @return Comment if the current index is inside a comment.
   */
  public PageElementComment isInComment(int currentIndex) {
    List<PageElementComment> tmpComments = getComments();
    for (PageElementComment comment : tmpComments) {
      if ((comment.getBeginIndex() <= currentIndex) &&
          (comment.getEndIndex() > currentIndex)) {
        return comment;
      }
    }
    return null;
  }

  // ==========================================================================
  // Titles management
  // ==========================================================================

  /**
   * All titles in the page.
   */
  private List<PageElementTitle> titles;

  /**
   * @return All titles in the page.
   */
  public List<PageElementTitle> getTitles() {
    thirdLevelAnalysis();
    return titles;
  }

  /**
   * @param currentIndex Current index.
   * @return Next title.
   */
  public PageElementTitle getNextTitle(int currentIndex) {
    List<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if (title.getBeginIndex() >= currentIndex) {
        return title;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Title if the current index is inside a title.
   */
  public PageElementTitle isInTitle(int currentIndex) {
    List<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if ((title.getBeginIndex() <= currentIndex) &&
          (title.getEndIndex() > currentIndex)) {
        return title;
      }
    }
    return null;
  }

  // ==========================================================================
  // Internal links management
  // ==========================================================================

  /**
   * All internal links in the page.
   */
  private List<PageElementInternalLink> internalLinks;

  /**
   * @return All internal links in the page.
   */
  public List<PageElementInternalLink> getInternalLinks() {
    thirdLevelAnalysis();
    return internalLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next internal link.
   */
  public PageElementInternalLink getNextInternalLink(int currentIndex) {
    List<PageElementInternalLink> tmpInternalLinks = getInternalLinks();
    for (PageElementInternalLink link : tmpInternalLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Internal link if the current index is inside an internal link.
   */
  public PageElementInternalLink isInInternalLink(int currentIndex) {
    List<PageElementInternalLink> tmpLinks = getInternalLinks();
    for (PageElementInternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  /**
   * Links count.
   */
  private Map<String, InternalLinkCount> linksCount = new HashMap<String, InternalLinkCount>();

  /**
   * @param link Link.
   * @return Number of links to the page.
   */
  public InternalLinkCount getLinkCount(Page link) {
    InternalLinkCount result = linksCount.get(link.getTitle());
    if (result != null) {
      return result;
    }
    List<Page> links = Collections.singletonList(link);
    InternalLinkCounter counter = new InternalLinkCounter(linksCount, links);
    PageAnalysisUtils.findInternalLinks(this, links, counter);
    return linksCount.get(link.getTitle());
  }

  /**
   * Count number of links in the page.
   * 
   * @param links Links.
   */
  public void countLinks(List<Page> links) {
    if ((links == null) || (links.size() == 0)) {
      return;
    }
    List<Page> interestingLinks = new ArrayList<Page>();
    for (Page link : links) {
      if (!linksCount.containsKey(link.getTitle())) {
        interestingLinks.add(link);
      }
    }
    if (interestingLinks.size() > 0) {
      InternalLinkCounter counter = new InternalLinkCounter(linksCount, interestingLinks);
      PageAnalysisUtils.findInternalLinks(this, interestingLinks, counter);
    }
  }

  // ==========================================================================
  // Images management
  // ==========================================================================

  /**
   * All images in the page.
   */
  private List<PageElementImage> images;

  /**
   * @return All images in the page.
   */
  public List<PageElementImage> getImages() {
    thirdLevelAnalysis();
    return images;
  }

  /**
   * @param currentIndex Current index.
   * @return Next image.
   */
  public PageElementImage getNextImage(int currentIndex) {
    List<PageElementImage> tmpImages = getImages();
    for (PageElementImage image : tmpImages) {
      if (image.getBeginIndex() >= currentIndex) {
        return image;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Image if the current index is inside an image.
   */
  public PageElementImage isInImage(int currentIndex) {
    List<PageElementImage> tmpImages = getImages();
    for (PageElementImage image : tmpImages) {
      if ((image.getBeginIndex() <= currentIndex) &&
          (image.getEndIndex() > currentIndex)) {
        return image;
      }
    }
    return null;
  }

  // ==========================================================================
  // External links management
  // ==========================================================================

  /**
   * All external links in the page.
   */
  private List<PageElementExternalLink> externalLinks;

  /**
   * @return All external links in the page.
   */
  public List<PageElementExternalLink> getExternalLinks() {
    thirdLevelAnalysis();
    return externalLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next external link.
   */
  public PageElementExternalLink getNextExternalLink(int currentIndex) {
    List<PageElementExternalLink> tmpExternalLinks = getExternalLinks();
    for (PageElementExternalLink link : tmpExternalLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return External link if the current index is inside an external link.
   */
  public PageElementExternalLink isInExternalLink(int currentIndex) {
    List<PageElementExternalLink> tmpLinks = getExternalLinks();
    for (PageElementExternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  // ==========================================================================
  // Templates management
  // ==========================================================================

  /**
   * All templates in the page.
   */
  private List<PageElementTemplate> templates;

  /**
   * @return All templates in the page.
   */
  public List<PageElementTemplate> getTemplates() {
    thirdLevelAnalysis();
    return templates;
  }

  /**
   * @param name Template name.
   * @return All templates with this name in the page analysis.
   */
  public List<PageElementTemplate> getTemplates(String name) {
    if (name == null) {
      return null;
    }
    List<PageElementTemplate> tmpTemplates = getTemplates();
    List<PageElementTemplate> result = new ArrayList<PageElementTemplate>();
    if (tmpTemplates != null) {
      for (PageElementTemplate template : tmpTemplates) {
        if (Page.areSameTitle(name, template.getTemplateName())) {
          result.add(template);
        }
      }
    }
    return result;
  }

  /**
   * @param currentIndex Current index.
   * @return Next template.
   */
  public PageElementTemplate getNextTemplate(int currentIndex) {
    List<PageElementTemplate> tmpTemplates = getTemplates();
    for (PageElementTemplate template : tmpTemplates) {
      if (template.getBeginIndex() >= currentIndex) {
        return template;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Template if the current index is inside a template.
   */
  public PageElementTemplate isInTemplate(int currentIndex) {
    List<PageElementTemplate> tmpTemplates = getTemplates();
    PageElementTemplate result = null;
    for (PageElementTemplate template : tmpTemplates) {
      if ((template.getBeginIndex() <= currentIndex) &&
          (template.getEndIndex() > currentIndex)) {
        result = template;
      }
    }
    return result;
  }

  // ==========================================================================
  // Tags management
  // ==========================================================================

  /**
   * All tags in the page.
   */
  private List<PageElementTag> tags;

  /**
   * Lock for updating the tags categorized by name.
   */
  private final Object lockTagsByName = new Object();

  /**
   * All tags in the page categorized by name.
   */
  private Map<String, List<PageElementTag>> tagsByName;

  /**
   * All complete tags in the page categorized by name.
   * Complete tags are either full tags or opening tags associated with a closing tag.
   */
  private Map<String, List<PageElementTag>> completeTagsByName;

  /**
   * @return All tags in the page.
   */
  public List<PageElementTag> getTags() {
    secondLevelAnalysis();
    return tags;
  }

  /**
   * @param name Tag name.
   * @return All tags with this name in the page.
   */
  public List<PageElementTag> getTags(String name) {
    if (name == null) {
      return null;
    }
    synchronized (lockTagsByName) {
      if (tagsByName == null) {
        tagsByName = new HashMap<String, List<PageElementTag>>();
      }
      name = name.toLowerCase();
      List<PageElementTag> result = tagsByName.get(name);
      if (result == null) {
        List<PageElementTag> tmpTags = getTags();
        result = new ArrayList<PageElementTag>();
        tagsByName.put(name, result);
        for (PageElementTag tag : tmpTags) {
          if (name.equals(tag.getNormalizedName())) {
            result.add(tag);
          }
        }
      }
      return result;
    }
  }

  /**
   * @param name Tag name.
   * @return All complete tags with this name in the page.
   */
  public List<PageElementTag> getCompleteTags(String name) {
    if (name == null) {
      return null;
    }
    synchronized (lockTagsByName) {
      if (completeTagsByName == null) {
        completeTagsByName = new HashMap<String, List<PageElementTag>>();
      }
      name = name.toLowerCase();
      List<PageElementTag> result = completeTagsByName.get(name);
      if (result == null) {
        List<PageElementTag> tmpTags = getTags(name);
        result = new ArrayList<PageElementTag>();
        completeTagsByName.put(name, result);
        for (PageElementTag tag : tmpTags) {
          if (tag.isFullTag()) {
            result.add(tag);
          } else if (!tag.isEndTag() && tag.isComplete()) {
            result.add(tag);
          }
        }
      }
      return result;
    }
  }

  /**
   * @param name Tag name.
   * @param currentIndex Current index.
   * @return Surrounding tag.
   */
  public PageElementTag getSurroundingTag(String name, int currentIndex) {
    List<PageElementTag> tmpTags = getCompleteTags(name);
    if (tmpTags == null) {
      return null;
    }
    PageElementTag result = null;
    for (PageElementTag tag : tmpTags) {
      if ((!tag.isFullTag()) &&
          (tag.getValueBeginIndex() <= currentIndex) &&
          (tag.getValueEndIndex() > currentIndex)) {
        if ((result == null) ||
            (tag.getBeginIndex() > result.getBeginIndex())) {
          result = tag;
        }
      }
    }
    return result;
  }

  /**
   * @param currentIndex Current index.
   * @return Next tag.
   */
  public PageElementTag getNextTag(int currentIndex) {
    List<PageElementTag> tmpTags = getTags();
    for (PageElementTag tag : tmpTags) {
      if (tag.getBeginIndex() >= currentIndex) {
        return tag;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Tag if the current index is inside a tag.
   */
  public PageElementTag isInTag(int currentIndex) {
    List<PageElementTag> tmpTags = getTags();
    for (PageElementTag tag : tmpTags) {
      if ((tag.getBeginIndex() <= currentIndex) &&
          (tag.getEndIndex() > currentIndex)) {
        return tag;
      }
    }
    return null;
  }

  // ==========================================================================
  // DEFAULTSORT management
  // ==========================================================================

  /**
   * All DEFAULTSORT in the page.
   */
  private List<PageElementDefaultsort> defaultSorts;

  /**
   * @return All DEFAULTSORT in the page.
   */
  public List<PageElementDefaultsort> getDefaultSorts() {
    thirdLevelAnalysis();
    return defaultSorts;
  }

  /**
   * @param currentIndex Current index.
   * @return Next DEFAULTSORT.
   */
  public PageElementDefaultsort getNextDefaultSort(int currentIndex) {
    List<PageElementDefaultsort> tmpDefaultSorts = getDefaultSorts();
    for (PageElementDefaultsort defaultSort : tmpDefaultSorts) {
      if (defaultSort.getBeginIndex() >= currentIndex) {
        return defaultSort;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return DefaultSort if the current index is inside a DEFAULTSORT.
   */
  public PageElementDefaultsort isInDefaultSort(int currentIndex) {
    List<PageElementDefaultsort> tmpDefaultSorts = getDefaultSorts();
    for (PageElementDefaultsort defaultSort : tmpDefaultSorts) {
      if ((defaultSort.getBeginIndex() <= currentIndex) &&
          (defaultSort.getEndIndex() > currentIndex)) {
        return defaultSort;
      }
    }
    return null;
  }

  // ==========================================================================
  // Categories management
  // ==========================================================================

  /**
   * All categories in the page.
   */
  private List<PageElementCategory> categories;

  /**
   * @return All categories in the page.
   */
  public List<PageElementCategory> getCategories() {
    thirdLevelAnalysis();
    return categories;
  }

  /**
   * @param currentIndex Current index.
   * @return Next category.
   */
  public PageElementCategory getNextCategory(int currentIndex) {
    List<PageElementCategory> tmpCategories = getCategories();
    for (PageElementCategory category : tmpCategories) {
      if (category.getBeginIndex() >= currentIndex) {
        return category;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Category if the current index is inside a category.
   */
  public PageElementCategory isInCategory(int currentIndex) {
    List<PageElementCategory> tmpCategories = getCategories();
    for (PageElementCategory category : tmpCategories) {
      if ((category.getBeginIndex() <= currentIndex) &&
          (category.getEndIndex() > currentIndex)) {
        return category;
      }
    }
    return null;
  }

  // ==========================================================================
  // Interwiki links management
  // ==========================================================================

  /**
   * All interwiki links in the page.
   */
  private List<PageElementInterwikiLink> interwikiLinks;

  /**
   * @return All interwiki links in the page.
   */
  public List<PageElementInterwikiLink> getInterwikiLinks() {
    thirdLevelAnalysis();
    return interwikiLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next interwiki link.
   */
  public PageElementInterwikiLink getNextInterwikiLink(int currentIndex) {
    List<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
    for (PageElementInterwikiLink link : tmpLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Interwiki link if the current index is inside an interwiki link.
   */
  public PageElementInterwikiLink isInInterwikiLink(int currentIndex) {
    List<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
    for (PageElementInterwikiLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  // ==========================================================================
  // Language links management
  // ==========================================================================

  /**
   * All language links in the page.
   */
  private List<PageElementLanguageLink> languageLinks;

  /**
   * @return All language links in the page.
   */
  public List<PageElementLanguageLink> getLanguageLinks() {
    thirdLevelAnalysis();
    return languageLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next language link.
   */
  public PageElementLanguageLink getNextLanguageLink(int currentIndex) {
    List<PageElementLanguageLink> tmpLinks = getLanguageLinks();
    for (PageElementLanguageLink link : tmpLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Language link if the current index is inside a language link.
   */
  public PageElementLanguageLink isInLanguageLink(int currentIndex) {
    List<PageElementLanguageLink> tmpLinks = getLanguageLinks();
    for (PageElementLanguageLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  // ==========================================================================
  // Errors management
  // ==========================================================================

  /**
   * Bean for holding results about error detection.
   */
  public static class Result {

    /**
     * True if errors of this kind have been found.
     */
    private final boolean found;

    /**
     * List of errors found.
     */
    private final List<CheckErrorResult> errors;

    Result(boolean found, List<CheckErrorResult> errors) {
      this.found = found;
      this.errors = (errors != null) ? new ArrayList<CheckErrorResult>(errors) : null;
    }

    /**
     * @param results (Out) List of errors found.
     * @return True if errors of this kind have been found.
     */
    public boolean getErrors(List<CheckErrorResult> results) {
      if ((results != null) && (errors != null)) {
        results.addAll(errors);
      }
      return found;
    }
  }

  /**
   * Memorizing Check Wiki errors.
   */
  private Map<Integer, Result> checkWikiErrors;

  /**
   * Memorize Check Wiki erros.
   * 
   * @param errorNumber Error number.
   * @param found True if errors of this kind have been found.
   * @param errors List of errors found.
   */
  public void setCheckWikiErrors(int errorNumber, boolean found, List<CheckErrorResult> errors) {
    if (checkWikiErrors == null) {
      checkWikiErrors = new HashMap<Integer, PageAnalysis.Result>();
    }
    checkWikiErrors.put(Integer.valueOf(errorNumber), new Result(found, errors));
  }

  /**
   * @param errorNumber Error number.
   * @return Errors for this error number.
   */
  public Result getCheckWikiErrors(int errorNumber) {
    if (checkWikiErrors == null) {
      return null;
    }
    return checkWikiErrors.get(Integer.valueOf(errorNumber));
  }
}
