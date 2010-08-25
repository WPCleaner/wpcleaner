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
import java.util.List;

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.PageListWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for getting pages embedded in. 
 */
public class EmbeddedInWorker extends BasicWorker {

  private final List<Page> embeddedInList;
  private final List<Page> pages;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages.
   */
  public EmbeddedInWorker(EnumWikipedia wikipedia, BasicWindow window, List<Page> pages) {
    super(wikipedia, window);
    this.pages = pages;
    embeddedInList = new ArrayList<Page>();
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
          GT._("Help requested on pages"),
          embeddedInList, getWikipedia(), false);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveAllEmbeddedIn(getWikipedia(), pages);
      if (!shouldContinue()) {
        return null;
      }
      for (Page page : pages) {
        embeddedInList.addAll(page.getEmbeddedIn());
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}