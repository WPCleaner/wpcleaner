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

import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * A popup menu listener for Page lists. 
 */
public class AnalysisPageListPopupListener extends
    AbstractPageListPopupListener {

  /**
   * @param wikipedia Wikipedia
   * @param textPane Text pane.
   * @param window Window.
   */
  public AnalysisPageListPopupListener(
      EnumWikipedia wikipedia,
      MediaWikiPane textPane,
      BasicWindow window) {
    super(wikipedia, textPane, window);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener#createPopup(javax.swing.JPopupMenu, org.wikipediacleaner.api.data.Page)
   */
  @Override
  protected void createPopup(JPopupMenu popup, Page link) {
    popup.add(new JSeparator());
    if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
      MenuCreator.addReplaceAllLinksToMenu(popup, link, textPane);
    }
    MenuCreator.addRemoveAllLinksToMenu(popup, link, textPane);
    popup.add(new JSeparator());
    if (Boolean.FALSE.equals(link.isExisting())) {
      MenuCreator.addRedLinksAnalysisMenu(wikipedia, popup, link, textPane);
      popup.add(new JSeparator());
    }
    MenuCreator.addAnalyzeToMenu(wikipedia, popup, link);
    MenuCreator.addViewToMenu(wikipedia, popup, link);
    MenuCreator.addDisambiguationToMenu(wikipedia, popup, link);
    MenuCreator.addReloadLinksToMenu(wikipedia, popup, link, window);
    //MenuCreator.addPurgeCacheToMenu(popup, link, window);
    popup.add(new JSeparator());
    MenuCreator.addFindTextToMenu(popup, link, textPane);
    MenuCreator.addAnalyzeTemplatesToMenu(wikipedia, popup, page, link);
  }

}
