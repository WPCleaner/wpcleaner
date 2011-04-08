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
import java.util.Properties;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.Configuration;


/**
 * An action listener for analyzing templates of a page. 
 */
public class MarkBacklinkAction implements ActionListener {

  private final EnumWikipedia wikipedia;
  private final Page page;
  private final Page link;
  private final String mark;
  private final Properties backlinksProperties;

  /**
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param link Backlink.
   * @param mark Kind of mark.
   * @param backlinksProperties Backlinks properties.
   */
  public MarkBacklinkAction(
      EnumWikipedia wikipedia,
      Page page, Page link, String mark,
      Properties backlinksProperties) {
    this.wikipedia = wikipedia;
    this.page = page;
    this.link = link;
    this.mark = mark;
    this.backlinksProperties = backlinksProperties;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if (mark == null) {
      backlinksProperties.remove(link.getTitle());
    } else {
      backlinksProperties.put(link.getTitle(), mark);
    }
    Configuration configuration = Configuration.getConfiguration();
    configuration.setSubProperties(
        wikipedia, Configuration.PROPERTIES_BACKLINKS, page.getTitle(), backlinksProperties);
  }
}
