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

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class FullAnalysisWorker extends BasicWorker {

  private final Page page;

  public FullAnalysisWorker(BasicWindow window, Page page) {
    super(window);
    this.page = page;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveContents(page, false, false);
      mw.retrieveAllLinks(page);
      mw.retrieveDisambiguationInformation(page.getLinks(), true);
      for (Page link : page.getLinks()) {
        if (Boolean.TRUE.equals(link.isDisambiguationPage()) &&
            link.hasWiktionaryTemplate()) {
          mw.retrieveContents(link, false, false);
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