/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.component;

import java.util.Properties;

import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * A popup menu listener for Page lists. 
 */
public class DisambiguationPageListPopupListener extends
    AbstractPageListPopupListener {

  private Properties backlinksProperties;

  /**
   * @param wikipedia Wikipedia
   * @param textPane Text pane.
   * @param window Window.
   */
  public DisambiguationPageListPopupListener(
      EnumWikipedia wikipedia,
      MWPane textPane,
      BasicWindow window) {
    super(wikipedia, textPane, window);
  }

  /**
   * @param properties Backlinks properties.
   */
  public void setBackLinksProperties(Properties properties) {
    backlinksProperties = properties;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener#createPopup(javax.swing.JPopupMenu, org.wikipediacleaner.api.data.Page)
   */
  @Override
  protected void createPopup(JPopupMenu popup, Page link) {
    if (backlinksProperties != null) {
      popup.add(new JSeparator());
      MenuCreator.addMarkBacklinkToMenu(wikipedia, popup, page, link, backlinksProperties);
    }
    popup.add(new JSeparator());
    MenuCreator.addAnalyzeToMenu(wikipedia, popup, link);
    MenuCreator.addViewToMenu(wikipedia, popup, link, true);
    MenuCreator.addDisambiguationToMenu(wikipedia, popup, link);
  }

}
