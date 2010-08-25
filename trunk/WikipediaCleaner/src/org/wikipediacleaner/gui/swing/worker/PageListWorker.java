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
import org.wikipediacleaner.api.base.APIException;
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

  private final List<String> pageNames;
  private final List<Page> pageList;
  private final String message;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pageNames List of pages in which the internal links have to be retrieved.
   * @param message Window title.
   */
  public PageListWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<String> pageNames, String message) {
    super(wikipedia, window);
    this.pageList = new ArrayList<Page>();
    this.pageNames = pageNames;
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
          message, pageList, getWikipedia(), false);
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
      mw.retrieveDisambiguationInformation(getWikipedia(), pages, null, false, true);
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