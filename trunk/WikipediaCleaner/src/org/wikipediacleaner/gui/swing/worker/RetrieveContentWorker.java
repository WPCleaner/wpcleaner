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

import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class RetrieveContentWorker extends BasicWorker {

  private final Page page;
  private final List<Page> pages;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   */
  public RetrieveContentWorker(EnumWikipedia wikipedia, BasicWindow window, Page page) {
    super(wikipedia, window);
    this.page = page;
    this.pages = null;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages.
   */
  public RetrieveContentWorker(EnumWikipedia wikipedia, BasicWindow window, List<Page> pages) {
    super(wikipedia, window);
    this.page = null;
    this.pages = pages;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      if (page != null) {
        mw.retrieveContents(getWikipedia(), page, true, false, true, false);
      } else {
        mw.retrieveContents(getWikipedia(), pages, true, true, false);
      }
      setText("Analyzing data");
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}