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

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class FullAnalysisWorker extends BasicWorker {

  private final Page page;
  private final ArrayList<Page> knownPages;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   * @param knownPages
   */
  public FullAnalysisWorker(
      EnumWikipedia wikipedia, BasicWindow window, Page page, ArrayList<Page> knownPages) {
    super(wikipedia, window);
    this.page = page;
    this.knownPages = knownPages;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveContents(getWikipedia(), page, false, false, true);
      mw.retrieveAllLinks(getWikipedia(), page, Namespace.MAIN, knownPages, true);
      mw.retrieveDisambiguationInformation(getWikipedia(), page.getLinks(), knownPages, true, true);
      for (Page link : page.getLinks()) {
        if (Boolean.TRUE.equals(link.isDisambiguationPage()) &&
            link.hasWiktionaryTemplate() &&
            (link.getContents() == null)) {
          mw.retrieveContents(getWikipedia(), link, false, false, true);
        }
      }
      mw.block(true);
      setText("Analyzing data");
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}