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

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.utils.Configuration;


/**
 * An analysis of a page.
 */
public class PageAnalysis {

  private final Page page;
  private final String contents;

  private boolean checkOrthograph;

  private final Object commentsLock = new Object();
  private Collection<PageElementComment> comments;
  private final Object titlesLock = new Object();
  private Collection<PageElementTitle> titles;
  private final Object internalLinksLock = new Object();
  private Collection<PageElementInternalLink> internalLinks;
  private final Object externalLinksLock = new Object();
  private Collection<PageElementExternalLink> externalLinks;
  private final Object templatesLock = new Object();
  private Collection<PageElementTemplate> templates;
  private final Object categoriesLock = new Object();
  private Collection<PageElementCategory> categories;
  private final Object interwikiLinksLock = new Object();
  private Collection<PageElementInterwikiLink> interwikiLinks;
  private final Object languageLinksLock = new Object();
  private Collection<PageElementLanguageLink> languageLinks;

  /**
   * @param page Page.
   * @param contents Page contents (may differ from page.getContents()).
   */
  public PageAnalysis(Page page, String contents) {
    this.page = page;
    this.contents = (contents != null) ? contents : page.getContents();

    // Default configuration
    Configuration config = Configuration.getConfiguration();
    checkOrthograph = config.getBoolean(
        null, Configuration.BOOLEAN_ORTHOGRAPH, Configuration.DEFAULT_ORTHOGRAPH);
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
   * @param check True if orthograph should be checked.
   */
  public void shouldCheckOrthograph(boolean check) {
    this.checkOrthograph = check;
  }

  /**
   * @return True if orthograph should be checked.
   */
  public boolean shouldCheckOrthograph() {
    return checkOrthograph;
  }

  /**
   * @return All comments in the page analysis.
   */
  public Collection<PageElementComment> getComments() {
    synchronized (commentsLock) {
      if (comments == null) {
        comments = PageContents.findAllComments(getWikipedia(), getContents());
      }
      return comments;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Comment if the current index is inside a comment.
   */
  public PageElementComment isInComment(int currentIndex) {
    Collection<PageElementComment> tmpComments = getComments();
    for (PageElementComment comment : tmpComments) {
      if ((comment.getBeginIndex() <= currentIndex) &&
          (comment.getEndIndex() > currentIndex)) {
        return comment;
      }
    }
    return null;
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

    return element;
  }

  /**
   * @return All titles in the page analysis.
   */
  public Collection<PageElementTitle> getTitles() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (titlesLock) {
      if (titles == null) {
        titles = PageContents.findAllTitles(getWikipedia(), getContents(), tmpComments);
      }
      return titles;
    }
  }

  /**
   * @param position Position in the text.
   * @return All titles leading to the given positio.
   */
  public Collection<PageElementTitle> getCurrentTitles(int position) {
    Collection<PageElementTitle> tmpTitles = getTitles();

    List<PageElementTitle> currentTitles = new ArrayList<PageElementTitle>();
    for (PageElementTitle title : tmpTitles) {
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
   * @param currentIndex Current index.
   * @return Next title.
   */
  public PageElementTitle getNextTitle(int currentIndex) {
    Collection<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if (title.getBeginIndex() >= currentIndex) {
        return title;
      }
    }
    return null;
  }

  /**
   * @return All internal links in the page analysis.
   */
  public Collection<PageElementInternalLink> getInternalLinks() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (internalLinksLock) {
      if (internalLinks == null) {
        internalLinks = PageContents.findAllInternalLinks(getPage(), getContents(), tmpComments);
      }
      return internalLinks;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Next internal link.
   */
  public PageElementInternalLink getNextInternalLink(int currentIndex) {
    Collection<PageElementInternalLink> tmpInternalLinks = getInternalLinks();
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
    Collection<PageElementInternalLink> tmpLinks = getInternalLinks();
    for (PageElementInternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  /**
   * @return All external links in the page analysis.
   */
  public Collection<PageElementExternalLink> getExternalLinks() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (externalLinksLock) {
      if (externalLinks == null) {
        externalLinks = PageContents.findAllExternalLinks(getPage(), getContents(), tmpComments);
      }
      return externalLinks;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Next external link.
   */
  public PageElementExternalLink getNextExternalLink(int currentIndex) {
    Collection<PageElementExternalLink> tmpExternalLinks = getExternalLinks();
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
    Collection<PageElementExternalLink> tmpLinks = getExternalLinks();
    for (PageElementExternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  /**
   * @return All templates in the page analysis.
   */
  public Collection<PageElementTemplate> getTemplates() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (templatesLock) {
      if (templates == null) {
        templates = PageContents.findAllTemplates(getPage(), getContents(), tmpComments);
      }
      return templates;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Next template.
   */
  public PageElementTemplate getNextTemplate(int currentIndex) {
    Collection<PageElementTemplate> tmpTemplates = getTemplates();
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
    Collection<PageElementTemplate> tmpTemplates = getTemplates();
    for (PageElementTemplate template : tmpTemplates) {
      if ((template.getBeginIndex() <= currentIndex) &&
          (template.getEndIndex() > currentIndex)) {
        return template;
      }
    }
    return null;
  }

  /**
   * @return All categories in the page analysis.
   */
  public Collection<PageElementCategory> getCategories() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (categoriesLock) {
      if (categories == null) {
        categories = PageContents.findAllCategories(getPage(), getContents(), tmpComments);
      }
      return categories;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Next category.
   */
  public PageElementCategory getNextCategory(int currentIndex) {
    Collection<PageElementCategory> tmpCategories = getCategories();
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
    Collection<PageElementCategory> tmpCategories = getCategories();
    for (PageElementCategory category : tmpCategories) {
      if ((category.getBeginIndex() <= currentIndex) &&
          (category.getEndIndex() > currentIndex)) {
        return category;
      }
    }
    return null;
  }

  /**
   * @return All interwiki links in the page analysis.
   */
  public Collection<PageElementInterwikiLink> getInterwikiLinks() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (interwikiLinksLock) {
      if (interwikiLinks == null) {
        interwikiLinks = PageContents.findAllInterwikiLinks(getPage(), getContents(), tmpComments);
      }
      return interwikiLinks;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Next interwiki link.
   */
  public PageElementInterwikiLink getNextInterwikiLink(int currentIndex) {
    Collection<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
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
    Collection<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
    for (PageElementInterwikiLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  /**
   * @return All language links in the page analysis.
   */
  public Collection<PageElementLanguageLink> getLanguageLinks() {
    Collection<PageElementComment> tmpComments = getComments();

    synchronized (languageLinksLock) {
      if (languageLinks == null) {
        languageLinks = PageContents.findAllLanguageLinks(getPage(), getContents(), tmpComments);
      }
      return languageLinks;
    }
  }

  /**
   * @param currentIndex Current index.
   * @return Next language link.
   */
  public PageElementLanguageLink getNextLanguageLink(int currentIndex) {
    Collection<PageElementLanguageLink> tmpLinks = getLanguageLinks();
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
    Collection<PageElementLanguageLink> tmpLinks = getLanguageLinks();
    for (PageElementLanguageLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }
}
