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

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.PageListWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for getting current disambiguation list. 
 */
public class DisambiguationListWorker extends BasicWorker {

  private final EnumWikipedia wikipedia;
  private final ArrayList<Page> disambiguationList;

  public DisambiguationListWorker(BasicWindow window, EnumWikipedia wikipedia) {
    super(window);
    this.wikipedia = wikipedia;
    disambiguationList = new ArrayList<Page>();
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
          GT._("Current disambiguation list"),
          disambiguationList, wikipedia, false);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      Page page = DataManager.getPage(wikipedia, wikipedia.getDisambiguationList(), null);
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveAllLinks(page);
      ArrayList<Page> pages = new ArrayList<Page>();
      Iterator<Page> iter = page.getLinks().iterator();
      while (iter.hasNext()) {
        Page link = iter.next();
        if ((link != null) && link.isInMainNamespace()) {
          pages.add(link);
        }
      }
      mw.retrieveDisambiguationInformation(pages, false);
      if (!shouldContinue()) {
        return null;
      }
      disambiguationList.addAll(pages);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}