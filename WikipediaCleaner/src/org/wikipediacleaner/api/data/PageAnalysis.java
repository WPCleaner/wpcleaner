/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
      boolean withExternalLinks, boolean withFunctions,
      boolean withImages, boolean withInternalLinks,
      boolean withInterwikiLinks, boolean withLanguageLinks,
      boolean withMagicWords, boolean withParameters,
      boolean withTags, boolean withTemplates, boolean withTitles) {
    List<PageElement> elements = new ArrayList<PageElement>();
    if (withCategories) {
      elements.addAll(getCategories());
    }
    if (withComments) {
      elements.addAll(getComments());
    }
    if (withExternalLinks) {
      elements.addAll(getExternalLinks());
    }
    if (withFunctions) {
      elements.addAll(getFunctions());
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
    if (withMagicWords) {
      elements.addAll(getMagicWords());
    }
    if (withParameters) {
      elements.addAll(getParameters());
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

    // Check if in parameter
    PageElementParameter parameter = isInParameter(currentIndex);
    if ((parameter != null) &&
        ((element == null) || (element.getBeginIndex() < parameter.getBeginIndex()))) {
      element = parameter;
    }

    // Check if in function
    PageElementFunction function = isInFunction(currentIndex);
    if ((function != null) &&
        ((element == null) || (element.getBeginIndex() < function.getBeginIndex()))) {
      element = function;
    }

    // Check if in ISBN
    PageElementISBN isbn = isInISBN(currentIndex);
    if ((isbn != null) &&
        ((element == null) || (element.getBeginIndex() < isbn.getBeginIndex()))) {
      element = isbn;
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
   * Internal lock for fourth level analysis.
   */
  private final Object fourthLevelLock = new Object();

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
      int maxIndex = (contents != null) ? contents.length() : 0;
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
      int maxIndex = (contents != null) ? contents.length() : 0;
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
      functions = new ArrayList<PageElementFunction>();
      magicWords = new ArrayList<PageElementMagicWord>();
      templates = new ArrayList<PageElementTemplate>();
      parameters = new ArrayList<PageElementParameter>();
      titles = new ArrayList<PageElementTitle>();
      externalLinks = new ArrayList<PageElementExternalLink>();

      // Go through all the text of the page
      int maxIndex = (contents != null) ? contents.length() : 0;
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
          } else if (contents.startsWith("{{{", currentIndex)) {
            currentIndex = analyze3CurlyBrackets(currentIndex);
          } else if (contents.startsWith("{{", currentIndex)) {
            currentIndex = analyze2CurlyBrackets(currentIndex);
          } else if (contents.startsWith("=", currentIndex)) {
            currentIndex = analyze1Equal(currentIndex);
          } else if (contents.startsWith("__", currentIndex)) {
            currentIndex = analyze2Undescore(currentIndex);
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
      areas.addFunctions(functions);
      areas.addMagicWords(magicWords);
      areas.addParameters(parameters);
      areas.addTitles(titles);
      areas.addExternalLinks(externalLinks);
    }
  }

  /**
   * Perform a fourth level analysis of the page (ISBN).
   */
  private void fourthLevelAnalysis() {
    synchronized (fourthLevelLock) {
      if ((isbns != null) || (issns != null) || (pmids != null)) {
        return;
      }
      thirdLevelAnalysis();
      isbns = PageElementISBN.analyzePage(this);
      areas.addISBN(isbns);
      issns = PageElementISSN.analyzePage(this);
      areas.addISSN(issns);
      pmids = PageElementPMID.analyzePage(this);
      areas.addPMID(pmids);
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
      return image.getBeginIndex() + 2 + image.getNamespace().length() + 1;
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
        getWikipedia(), contents, currentIndex, this);
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
   * Part of the third level of analysis when text is beginning with "__".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze2Undescore(int currentIndex) {

    // Check if this a magic word
    PageElementMagicWord magicWord = PageElementMagicWord.analyzeBlock(
        getWikipedia(), contents, currentIndex);
    if (magicWord != null) {
      magicWords.add(magicWord);
      return magicWord.getEndIndex();
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
          getWikipedia(), contents, currentIndex, this);
      if (link != null) {
        externalLinks.add(link);
        return link.getEndIndex();
      }
    }

    return currentIndex + 1;
  }

  /**
   * Part of the third level of analysis when text is beginning with "{{{".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze3CurlyBrackets(int currentIndex) {

    // Check if this is a parameter
    PageElementParameter parameter = PageElementParameter.analyzeBlock(
        getWikipedia(), contents, currentIndex, comments, tags);
    if (parameter != null) {
      parameters.add(parameter);
      return currentIndex + 3;
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

    // Check if this is a function
    PageElementFunction function = PageElementFunction.analyzeBlock(
        getWikipedia(), contents, currentIndex, comments, tags);
    if (function != null) {
      functions.add(function);
      if (function.getParameterCount() == 0) {
        return function.getEndIndex();
      }
      return currentIndex + 2;
    }

    // Check if this is a template
    PageElementTemplate template = PageElementTemplate.analyzeBlock(
        getWikipedia(), contents, currentIndex, comments, tags);
    if (template != null) {
      templates.add(template);
      if (template.getParameterCount() == 0) {
        return template.getEndIndex();
      }
      return currentIndex + 2;
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

    // Check that it's a beginning of a line
    if ((currentIndex > 0) && (contents.charAt(currentIndex - 1) != '\n')) {
      return currentIndex + 1;
    }

    // Check that it's not a template value
    if (templates != null) {
      PageElementTemplate template = null;
      for (PageElementTemplate tmp : templates) {
        if ((tmp.getBeginIndex() <= currentIndex) &&
            (tmp.getEndIndex() > currentIndex)) {
          template = tmp;
        }
      }
      if (template != null) {
        for (int i = 0; i < template.getParameterCount(); i++) {
          int beginParam = template.getParameterPipeIndex(i);
          int endParam = template.getParameterValueStartIndex(i);
          if ((currentIndex >= beginParam) && (currentIndex < endParam)) {
            return currentIndex + 1;
          }
        }
      }
    }

    // Check if this is a title
    PageElementTitle title = PageElementTitle.analyzeBlock(
        getWikipedia(), contents, currentIndex, comments, tags);
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

  /**
   * @return True if titles seem to be reliable.
   */
  public boolean areTitlesReliable() {
    List<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if (!title.isCoherent()) {
        return false;
      }
      if (isInTemplate(title.getBeginIndex()) != null) {
        return false;
      }
    }
    return true;
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
    PageElementImage result = null;
    for (PageElementImage image : tmpImages) {
      if ((image.getBeginIndex() <= currentIndex) &&
          (image.getEndIndex() > currentIndex)) {
        if ((result == null) ||
            (image.getBeginIndex() > result.getBeginIndex())) {
          result = image;
        }
      }
    }
    return result;
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
  // Parameters management
  // ==========================================================================

  /**
   * All parameters in the page.
   */
  private List<PageElementParameter> parameters;

  /**
   * @return All parameters in the page.
   */
  public List<PageElementParameter> getParameters() {
    thirdLevelAnalysis();
    return parameters;
  }

  /**
   * @param currentIndex Current index.
   * @return Parameter if the current index is inside a parameter.
   */
  public PageElementParameter isInParameter(int currentIndex) {
    List<PageElementParameter> tmpParameters = getParameters();
    PageElementParameter result = null;
    for (PageElementParameter parameter : tmpParameters) {
      if ((parameter.getBeginIndex() <= currentIndex) &&
          (parameter.getEndIndex() > currentIndex)) {
        result = parameter;
      }
    }
    return result;
  }

  // ==========================================================================
  // Functions management
  // ==========================================================================

  /**
   * All functions in the page.
   */
  private List<PageElementFunction> functions;

  /**
   * @return All functions in the page.
   */
  public List<PageElementFunction> getFunctions() {
    thirdLevelAnalysis();
    return functions;
  }

  /**
   * @param currentIndex Current index.
   * @return Function if the current index is inside a function.
   */
  public PageElementFunction isInFunction(int currentIndex) {
    List<PageElementFunction> tmpFunctions = getFunctions();
    PageElementFunction result = null;
    for (PageElementFunction function : tmpFunctions) {
      if ((function.getBeginIndex() <= currentIndex) &&
          (function.getEndIndex() > currentIndex)) {
        result = function;
      }
    }
    return result;
  }

  // ==========================================================================
  // Magic words management
  // ==========================================================================

  /**
   * All magic words in the page.
   */
  private List<PageElementMagicWord> magicWords;

  /**
   * @return All magic words in the page.
   */
  public List<PageElementMagicWord> getMagicWords() {
    thirdLevelAnalysis();
    return magicWords;
  }

  /**
   * @param currentIndex Current index.
   * @return Magic word if the current index is inside a magic word.
   */
  public PageElementMagicWord isInMagicWord(int currentIndex) {
    List<PageElementMagicWord> tmpMagicWords = getMagicWords();
    PageElementMagicWord result = null;
    for (PageElementMagicWord magicWord : tmpMagicWords) {
      if ((magicWord.getBeginIndex() <= currentIndex) &&
          (magicWord.getEndIndex() > currentIndex)) {
        result = magicWord;
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

  /**
   * @param currentIndex Current index.
   * @param tagName Tag name.
   * @return Tag if the current index is inside a tag.
   */
  public PageElementTag isInTag(int currentIndex, String tagName) {
    List<PageElementTag> tmpTags = getTags(tagName);
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
   * @return All DEFAULTSORT in the page.
   */
  public List<PageElementFunction> getDefaultSorts() {
    List<PageElementFunction> tmpFunctions = getFunctions();
    if (tmpFunctions == null) {
      return null;
    }
    List<PageElementFunction> defaultSorts = new ArrayList<PageElementFunction>();
    for (PageElementFunction function : tmpFunctions) {
      if (MagicWord.DEFAULT_SORT.equals(function.getMagicWord().getName())) {
        defaultSorts.add(function);
      }
    }
    return defaultSorts;
  }

  /**
   * @param currentIndex Current index.
   * @return DefaultSort if the current index is inside a DEFAULTSORT.
   */
  public PageElementFunction isInDefaultSort(int currentIndex) {
    List<PageElementFunction> tmpDefaultSorts = getDefaultSorts();
    for (PageElementFunction defaultSort : tmpDefaultSorts) {
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
  // ISBN, ISSN and PMID management
  // ==========================================================================

  /**
   * All ISBNs in the page
   */
  private List<PageElementISBN> isbns;

  /**
   * @return All ISBNs in the page.
   */
  public List<PageElementISBN> getISBNs() {
    fourthLevelAnalysis();
    return isbns;
  }

  /**
   * @param currentIndex Current index.
   * @return ISBN if the current index is inside an ISBN.
   */
  public PageElementISBN isInISBN(int currentIndex) {
    List<PageElementISBN> tmpIsbns = getISBNs();
    for (PageElementISBN isbn : tmpIsbns) {
      if ((isbn.getBeginIndex() <= currentIndex) &&
          (isbn.getEndIndex() > currentIndex)) {
        return isbn;
      }
    }
    return null;
  }



  /**
   * All ISSNs in the page
   */
  private List<PageElementISSN> issns;

  /**
   * @return All ISSNs in the page.
   */
  public List<PageElementISSN> getISSNs() {
    fourthLevelAnalysis();
    return issns;
  }

  /**
   * @param currentIndex Current index.
   * @return ISSN if the current index is inside an ISSN.
   */
  public PageElementISSN isInISSN(int currentIndex) {
    List<PageElementISSN> tmpIsbns = getISSNs();
    for (PageElementISSN issn : tmpIsbns) {
      if ((issn.getBeginIndex() <= currentIndex) &&
          (issn.getEndIndex() > currentIndex)) {
        return issn;
      }
    }
    return null;
  }

  
  /**
   * All PMIDs in the page
   */
  private List<PageElementPMID> pmids;

  /**
   * @return All PMIDs in the page.
   */
  public List<PageElementPMID> getPMIDs() {
    fourthLevelAnalysis();
    return pmids;
  }

  /**
   * @param currentIndex Current index.
   * @return PMID if the current index is inside a PMID.
   */
  public PageElementPMID isInPMID(int currentIndex) {
    List<PageElementPMID> tmpPmids = getPMIDs();
    for (PageElementPMID pmid : tmpPmids) {
      if ((pmid.getBeginIndex() <= currentIndex) &&
          (pmid.getEndIndex() > currentIndex)) {
        return pmid;
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
   * Memorize Check Wiki errors.
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
