/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class DisambiguationAnalysisWorker extends BasicWorker {

  private final Page page;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   */
  public DisambiguationAnalysisWorker(EnumWikipedia wikipedia, BasicWindow window, Page page) {
    super(wikipedia, window);
    this.page = page;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveContents(getWikipedia(), page, false, false, false, true);
      mw.retrieveAllBacklinks(getWikipedia(), page, true);
      ArrayList<Page> pageAndRedirects = new ArrayList<Page>();
      pageAndRedirects.add(page);
      for (Page backlink : page.getBackLinksWithRedirects()) {
        if ((backlink != null) && (backlink.isRedirect())) {
          pageAndRedirects.add(backlink);
          mw.retrieveContents(getWikipedia(), backlink, false, false, false, false);
        }
      }
      mw.retrieveDisambiguationInformation(getWikipedia(), pageAndRedirects, null, false, false, false);
      mw.retrieveAllLinks(getWikipedia(), page, null, null, true, false);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}