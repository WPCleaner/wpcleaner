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

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;


/**
 * An action listener for viewing page. 
 */
public class PageViewAction implements ActionListener {

  private final String title;
  private final EnumWikipedia wiki;
  private final String action;

  public PageViewAction(String url) {
    this(url, null, null);
  }

  public PageViewAction(String title, EnumWikipedia wiki) {
    this(title, wiki, null);
  }

  public PageViewAction(String title, EnumWikipedia wiki, String action) {
    this.title = title;
    this.wiki = wiki;
    this.action = action;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if (action != null) {
      Utilities.browseURL(wiki, title, action);
    } else if (wiki != null) {
      Utilities.browseURL(wiki, title, false);
    } else {
      Utilities.browseURL(title);
    }
  }
}
