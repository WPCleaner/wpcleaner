/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumQueryPage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.PageListWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;

/**
 * SwingWorker for getting various list of pages. 
 */
public class PageListWorker extends BasicWorker {

  /**
   * The <code>Mode</code> allows to specify how the PageListWorker
   * will use the list of elements provided to it:
   * <ul>
   * <li>ALL_DAB_PAGES: Not used.
   *     Retrieve list of all disambiguation pages.</li>
   * <li>CATEGORY_MEMBERS: List of categories.
   *     Retrieve list of articles in the categories.</li>
   * <li>CATEGORY_MEMBERS_ARTICLES: List of categories.
   *     Retrieve list of articles in the categories.
   *     If talk pages are found, the related article is used instead.</li>
   * <li>DAB_WATCH: List of disambiguation pages.
   *     Retrieve list of pages linking to the disambiguation pages and needing attention.</li>
   * <li>DIRECT: Direct list</li>
   * <li>EMBEDDED_IN: List of templates.
   *     Retrieve list of pages embedding the templates.</li>
   * <li>INTERNAL_LINKS: List of pages.
   *     Retrieve list of internal links in the pages.</li>
   * <li>MISSING_TEMPLATES: Not used.
   *     Retrieve list of pages with missing templates.</li>
   * <li>QUERY_PAGE: Code of the special list to retrieve.
   *     Retrieve list of pages of a special list.</li>
   * <li>SEARCH_TITLES: List of keywords.
   *     Retrieve list of pages matching keywords.</li>
   * <li>WATCH_LIST: Not used.
   *     Retrieve list of pages in the watch list.</li>
   * </ul>
   */
  public static enum Mode {
    ALL_DAB_PAGES,
    CATEGORY_MEMBERS,
    CATEGORY_MEMBERS_ARTICLES,
    DAB_WATCH,
    DIRECT,
    EMBEDDED_IN,
    INTERNAL_LINKS,
    MISSING_TEMPLATES,
    QUERY_PAGE,
    SEARCH_TITLES,
    WATCH_LIST,
  }

  private final Page referencePage;
  private final List<String> elementNames;
  private final Mode mode;
  private final boolean watchList;
  private final List<Page> pageList;
  private final String message;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param elementNames List of elements (page names, ...).
   * @param mode Mode for determining the list of pages.
   * @param message Window title.
   */
  public PageListWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      Page referencePage,
      List<String> elementNames, Mode mode,
      boolean watchList, String message) {
    super(wikipedia, window);
    this.referencePage = referencePage;
    this.pageList = new ArrayList<Page>();
    this.elementNames = elementNames;
    this.mode = mode;
    this.watchList = watchList;
    this.message = message;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    Object result = get();
    if (!(result instanceof Throwable)) {
      if (mode == Mode.ALL_DAB_PAGES) {
        Set<String> set = new HashSet<String>(pageList.size());
        for (Page page : pageList) {
          set.add(page.getTitle());
        }
        getWikipedia().setDisambiguationPages(set);
        int answer = Utilities.displayYesNoWarning(
            (getWindow() != null) ? getWindow().getParentComponent() : null,
            GT._(
                "You have loaded the list of all disambiguation pages to speed up page analysis.\n" +
                "Do you want to display the list of all disambiguation pages ?"));
        if (answer != JOptionPane.YES_OPTION) {
          return;
        }
      }
      PageListWindow.createPageListWindow(
          message, referencePage, pageList, getWikipedia(), watchList);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      List<Page> pages = new ArrayList<Page>();
      boolean retrieveDisambiguationInformation = true;
      switch (mode) {

      // List all disambiguations pages
      case ALL_DAB_PAGES:
        constructAllDab(pages);
        retrieveDisambiguationInformation = false;
        break;

      // List members of a category
      case CATEGORY_MEMBERS:
        constructCategoryMembers(pages);
        break;

        // List article members of a category
      case CATEGORY_MEMBERS_ARTICLES:
        constructCategoryMembersArticles(pages);
        break;

      // List pages with disambiguation links requiring attention
      case DAB_WATCH:
        constructDabWatch(pages);
        break;

      // List pages embedding a template
      case EMBEDDED_IN:
        constructEmbeddedIn(pages);
        break;

      // List internal links in a page
      case INTERNAL_LINKS:
        constructInternalLinks(pages);
        break;

      // Retrieve list of pages with missing templates
      case MISSING_TEMPLATES:
        constructMissingTemplates(pages);
        break;

      // Retrieve a special list
      case QUERY_PAGE:
        constructQueryPage(pages);
        break;

      // Search similar pages
      case SEARCH_TITLES:
        constructSearchTitles(pages);
        break;

      // List pages in the watch list
      case WATCH_LIST:
        constructWatchList(pages);
        break;

      default:
        pages.addAll(constructInternalPageList());
        break;
      }

      if (retrieveDisambiguationInformation) {
        MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
        mw.retrieveDisambiguationInformation(getWikipedia(), pages, null, false, true, true);
      }
      if (!shouldContinue()) {
        return null;
      }
      pageList.addAll(pages);
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * Construct the list of pages from the list of page names.
   * 
   * @return Internal list of pages.
   */
  private List<Page> constructInternalPageList() {
    if (elementNames == null) {
      return new ArrayList<Page>();
    }
    List<Page> pages = new ArrayList<Page>(elementNames.size());
    for (String pageName : elementNames) {
      pages.add(DataManager.getPage(getWikipedia(), pageName, null, null, null));
    }
    return pages;
  }

  /**
   * Construct list of all disambiguation pages.
   * 
   * @param pages List of all disambiguation pages.
   * @throws APIException
   */
  private void constructAllDab(List<Page> pages) throws APIException {
    final API api = APIFactory.getAPI();
    EnumWikipedia wiki = getWikipedia();
    List<Page> tmpPages = wiki.constuctDisambiguationPages(api);
    if (tmpPages != null) {
      pages.addAll(tmpPages);
    }
  }

  /**
   * Construct list of pages members of the categories.
   * 
   * @param pages List of pages members of the categories.
   * @throws APIException
   */
  private void constructCategoryMembers(List<Page> pages) throws APIException {
    final API api = APIFactory.getAPI();
    for (String pageName : elementNames) {
      List<Page> tmpPages = api.retrieveCategoryMembers(getWikipedia(), pageName, 0, true);
      if (tmpPages != null) {
        for (Page tmpPage : tmpPages) {
          if (!pages.contains(tmpPage)) {
            pages.add(tmpPage);
          }
        }
      }
    }
  }

  /**
   * Construct list of articles members of the categories.
   * 
   * @param pages List of articles members of the categories.
   * @throws APIException
   */
  private void constructCategoryMembersArticles(List<Page> pages) throws APIException {
    final API api = APIFactory.getAPI();
    for (String pageName : elementNames) {
      List<Page> tmpPages = api.retrieveCategoryMembers(getWikipedia(), pageName, 0, true);
      if (tmpPages != null) {
        WPCConfiguration configuration = getWikipedia().getConfiguration();
        for (Page tmpPage : tmpPages) {
          if (!tmpPage.isArticle()) {
            String title = tmpPage.getArticlePageName();
            String todoSubpage = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
            if ((todoSubpage != null) &&
                (todoSubpage.trim().length() > 0) &&
                (title.endsWith("/" + todoSubpage))) {
              title = title.substring(0, title.length() - 1 - todoSubpage.length());
            }
            tmpPage = DataManager.getPage(getWikipedia(), title, null, null, null);
          }
          if (!pages.contains(tmpPage)) {
            pages.add(tmpPage);
          }
        }
      }
    }
  }

  /**
   * Construct list of pages with disambiguation links requiring attention.
   * 
   * @param pages List of disambiguation pages.
   * @throws APIException
   */
  private void constructDabWatch(List<Page> pages) throws APIException {
    if (elementNames != null) {
      List<Page> tmpPages = constructInternalPageList();
      Page[] tmpPages2 = new Page[tmpPages.size()];
      tmpPages2 = tmpPages.toArray(tmpPages2);
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveAllBacklinks(getWikipedia(), tmpPages2, true);
      Configuration configuration = Configuration.getConfiguration();
      for (Page tmpPage : tmpPages2) {
        List<Page> backlinks = tmpPage.getBackLinksWithRedirects();
        if (backlinks != null) {
          Properties pageProperties = configuration.getSubProperties(
              getWikipedia(), Configuration.PROPERTIES_BACKLINKS, tmpPage.getTitle());
          for (Page page : backlinks) {
            if ((pageProperties == null) ||
                (!pageProperties.containsKey(page.getTitle()))) {
              Integer namespace = page.getNamespace();
              if ((namespace != null) &&
                  ((namespace.intValue() == Namespace.MAIN) ||
                   (namespace.intValue() == Namespace.TEMPLATE))) {
                if (!pages.contains(page)) {
                  pages.add(page);
                }
              }
            }
          }
        }
      }
    }
  }

  /**
   * Construct list of pages embedding the templates.
   * 
   * @param pages List of pages embedding the templates.
   * @throws APIException
   */
  private void constructEmbeddedIn(List<Page> pages) throws APIException {
    if (elementNames != null) {
      List<Page> tmpPages = constructInternalPageList();
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      pages.addAll(mw.retrieveAllEmbeddedIn(getWikipedia(), tmpPages, null, true));
    }
  }

  /**
   * Construct list of internal links contained in the pages.
   * 
   * @param pages List of internal links in the pages.
   * @throws APIException
   */
  private void constructInternalLinks(List<Page> pages) throws APIException {
    for (String dabList : elementNames) {
      Page page = DataManager.getPage(getWikipedia(), dabList, null, null, null);
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveAllLinks(getWikipedia(), page, null, null, true);
      Iterator<Page> iter = page.getLinks().iterator();
      while (iter.hasNext()) {
        Page link = iter.next();
        if ((link != null) &&
            (link.isInMainNamespace()) &&
            (!pages.contains(link))) {
          pages.add(link);
        }
      }
    }
  }

  /**
   * Construct list of pages with missing templates.
   * 
   * @param pages List of pages with missing templates.
   * @throws APIException
   */
  private void constructMissingTemplates(List<Page> pages) throws APIException {
    final API api = APIFactory.getAPI();
    EnumWikipedia wiki = getWikipedia();
    setText(GT._("Retrieving list of missing templates"));
    List<Page> tmpPages = api.getQueryPages(wiki, EnumQueryPage.WANTED_TEMPLATES);
    if (tmpPages == null) {
      return;
    }
    setText(GT._("Checking that the templates are still missing"));
    api.retrieveInfo(wiki, tmpPages);
    List<Page> tmpPages2 = new ArrayList<Page>();
    for (Page tmpPage : tmpPages) {
      Boolean exists = tmpPage.isExisting();
      if (!Boolean.TRUE.equals(exists)) {
        tmpPages2.add(tmpPage);
      }
    }
    MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
    List<Page> tmpPages3 = mw.retrieveAllEmbeddedIn(
        wiki, tmpPages2,
        wiki.getConfiguration().getEncyclopedicNamespaces(), true);
    pages.addAll(tmpPages3);
    Collections.sort(pages, PageComparator.getNamespaceFirstComparator());
  }

  /**
   * Construct special list of pages.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  private void constructQueryPage(List<Page> pages) throws APIException {
    final API api = APIFactory.getAPI();
    EnumWikipedia wiki = getWikipedia();
    EnumQueryPage query = EnumQueryPage.findByCode(elementNames.get(0));
    List<Page> tmpPages = api.getQueryPages(wiki, query);
    if (tmpPages != null) {
      pages.addAll(tmpPages);
    }
  }

  /**
   * Construct list of search results.
   * 
   * @param pages List of search results.
   * @throws APIException
   */
  private void constructSearchTitles(List<Page> pages) throws APIException {
    if (elementNames != null) {
      final API api = APIFactory.getAPI();
      for (String pageName : elementNames) {
        Page page = DataManager.getPage(getWikipedia(), pageName, null, null, null);
        api.retrieveSimilarPages(getWikipedia(), page, true);
        pages.addAll(page.getSimilarPages());
      }
    }
  }

  /**
   * Construct list of pages in the watch list.
   * 
   * @param pages List of pages in the watch list.
   * @throws APIException
   */
  private void constructWatchList(List<Page> pages) throws APIException {
    final API api = APIFactory.getAPI();
    List<Page> tmpPages = api.retrieveRawWatchlist(getWikipedia());
    if (tmpPages != null) {
      pages.addAll(tmpPages);
    }
  }
}