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
import java.util.Iterator;
import java.util.List;

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.PageListWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for getting current disambiguation list. 
 */
public class PageListWorker extends BasicWorker {

  public static enum Mode {
    ALL_DAB_PAGES,
    CATEGORY_MEMBERS,
    DIRECT,
    EMBEDDED_IN,
    INTERNAL_LINKS,
  }

  private final List<String> pageNames;
  private final Mode mode;
  private final boolean watchList;
  private final List<Page> pageList;
  private final String message;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pageNames List of pages.
   * @param mode Mode for determining the list of pages.
   * @param message Window title.
   */
  public PageListWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<String> pageNames, Mode mode,
      boolean watchList, String message) {
    super(wikipedia, window);
    this.pageList = new ArrayList<Page>();
    this.pageNames = pageNames;
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
      PageListWindow.createPageListWindow(
          message, pageList, getWikipedia(), watchList);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      List<Page> pages = new ArrayList<Page>();
      final API api = APIFactory.getAPI();
      boolean retrieveDisambiguationInformation = true;
      switch (mode) {

      // List all disambiguations pages
      case ALL_DAB_PAGES:
        if (pageNames != null) {
          List<Page> tmpPages = new ArrayList<Page>(pageNames.size());
          for (String pageName : pageNames) {
            tmpPages.add(DataManager.getPage(getWikipedia(), pageName, null, null));
          }
          tmpPages = mw.retrieveAllEmbeddedIn(getWikipedia(), tmpPages);
          for (Page page : tmpPages) {
            if (page.isInMainNamespace()) {
              page.setDisambiguationPage(Boolean.TRUE);
              pages.add(page);
            }
          }
          retrieveDisambiguationInformation = false;
        }
        break;

      // List members of a category
      case CATEGORY_MEMBERS:
        for (String pageName : pageNames) {
          List<Page> tmpPages = api.retrieveCategoryMembers(getWikipedia(), pageName, 0);
          if (tmpPages != null) {
            for (Page tmpPage : tmpPages) {
              if (!pages.contains(tmpPage)) {
                pages.add(tmpPage);
              }
            }
          }
        }
        break;

      // List pages embedding a template
      case EMBEDDED_IN:
        if (pageNames != null) {
          List<Page> tmpPages = new ArrayList<Page>(pageNames.size());
          for (String pageName : pageNames) {
            tmpPages.add(DataManager.getPage(getWikipedia(), pageName, null, null));
          }
          pages.addAll(mw.retrieveAllEmbeddedIn(getWikipedia(), tmpPages));
        }
        break;

      // List internal links in a page
      case INTERNAL_LINKS:
        for (String dabList : pageNames) {
          Page page = DataManager.getPage(getWikipedia(), dabList, null, null);
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
        break;

      default:
        for (String page : pageNames) {
          pages.add(DataManager.getPage(getWikipedia(), page, null, null));
        }
        api.initializeRedirect(getWikipedia(), pages);
        break;
      }
      if (retrieveDisambiguationInformation) {
        mw.retrieveDisambiguationInformation(getWikipedia(), pages, null, false, true);
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
}