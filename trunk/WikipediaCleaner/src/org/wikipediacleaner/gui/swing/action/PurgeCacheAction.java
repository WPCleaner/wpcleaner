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

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.ProgressPanel;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for reloading links of a page. 
 */
public class PurgeCacheAction implements ActionListener {

  private final EnumWikipedia wikipedia;
  private final Page page;
  private final BasicWindow window;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param page Wikipedia page.
   * @param window Window on which the action is done.
   */
  public PurgeCacheAction(
      EnumWikipedia wikipedia,
      Page page,
      BasicWindow window) {
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    ProgressPanel progressPanel = (window != null) ? window.getGlassPane() : null;
    try {
      if (progressPanel != null) {
        progressPanel.start();
        progressPanel.setText(GT._("Retrieving MediaWiki API"));
      }
      API api = APIFactory.getAPI();
      if (progressPanel != null) {
        progressPanel.setText(GT._("Purging page cache"));
      }
      api.purgePageCache(wikipedia, page);
    } catch (APIException ex) {
      //
    } finally {
      if (progressPanel != null) {
        progressPanel.stop();
      }
    }
  }
}
