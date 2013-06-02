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

package org.wikipediacleaner.gui.swing.menu;

import java.util.Properties;

import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.action.MarkBacklinkAction;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A helper class to manage contextual menu.
 */
public class DisambiguationPageListMenuCreator extends BasicMenuCreator {

  /**
   * Add items for marking backlinks.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param link Backlink.
   * @param backlinks Backlinks properties.
   */
  public void addItemsMarkBacklink(
      EnumWikipedia wiki, JPopupMenu popup,
      Page page, Page link, Properties backlinks) {
    if ((page != null) &&
        (link != null) &&
        (wiki != null) &&
        (backlinks != null)) {
      String property = backlinks.getProperty(link.getTitle());
      if (!Configuration.VALUE_PAGE_NORMAL.equals(property)) {
        addItem(
            popup, null, GT._("Mark backlink as normal"), true,
            new MarkBacklinkAction(wiki, page, link, Configuration.VALUE_PAGE_NORMAL, backlinks));
      }
      if (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property)) {
        addItem(
            popup, null, GT._("Mark backlink as needing help"), true,
            new MarkBacklinkAction(wiki, page, link, Configuration.VALUE_PAGE_HELP_NEEDED, backlinks));
      }
      if ((Configuration.VALUE_PAGE_NORMAL.equals(property)) ||
          (Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
        addItem(
            popup, null, GT._("Remove mark on backlink"), true,
            new MarkBacklinkAction(wiki, page, link, null, backlinks));
      }
    }
  }
}
