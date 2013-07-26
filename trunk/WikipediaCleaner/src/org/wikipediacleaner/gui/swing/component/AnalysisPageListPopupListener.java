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

package org.wikipediacleaner.gui.swing.component;

import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.menu.AnalysisPageListMenuCreator;


/**
 * A popup menu listener for Page lists. 
 */
public class AnalysisPageListPopupListener extends
    AbstractPageListPopupListener {

  /**
   * @param wiki Wiki
   * @param textPane Text pane.
   * @param list List.
   * @param window Window.
   */
  public AnalysisPageListPopupListener(
      EnumWikipedia wiki,
      MWPane textPane, JList list,
      BasicWindow window) {
    super(wiki, textPane, list, window);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener#createPopup(javax.swing.JPopupMenu, org.wikipediacleaner.api.data.Page)
   */
  @Override
  protected void createPopup(JPopupMenu popup, Page link) {
    AnalysisPageListMenuCreator menu = new AnalysisPageListMenuCreator();
    menu.addSeparator(popup);
    if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
      menu.addReplaceAllLinks(popup, link, textPane);
    }
    menu.addItemRemoveAllLinks(popup, link, textPane);
    menu.addSeparator(popup);
    if (Boolean.FALSE.equals(link.isExisting())) {
      menu.addItemRedLinksAnalysis(wikipedia, popup, link, textPane);
      popup.add(new JSeparator());
    }
    menu.addAnalyze(wikipedia, popup, link);
    menu.addView(wikipedia, popup, link, true);
    menu.addDisambiguation(wikipedia, popup, link);
    menu.addItemReloadLinks(wikipedia, popup, link, window);
    //menu.addItemPurgeCache(popup, link, window);
    menu.addSeparator(popup);
    menu.addItemFindText(popup, link, textPane);
    menu.addItemAnalyzeTemplates(wikipedia, popup, page, link);
  }

}
