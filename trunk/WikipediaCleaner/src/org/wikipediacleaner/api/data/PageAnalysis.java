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

import java.util.Collection;

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
   * @return True if the current index is inside an internal link.
   */
  public boolean isInInternalLink(int currentIndex) {
    Collection<PageElementInternalLink> tmpLinks = getInternalLinks();
    for (PageElementInternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return true;
      }
    }
    return false;
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
   * @return True if the current index is inside an interwiki link.
   */
  public boolean isInInterwikiLink(int currentIndex) {
    Collection<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
    for (PageElementInterwikiLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return true;
      }
    }
    return false;
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
   * @return True if the current index is inside a language link.
   */
  public boolean isInLanguageLink(int currentIndex) {
    Collection<PageElementLanguageLink> tmpLinks = getLanguageLinks();
    for (PageElementLanguageLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return true;
      }
    }
    return false;
  }
}
