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

import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinkReplacement;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.action.FindTextAction;
import org.wikipediacleaner.gui.swing.action.RedLinksAnalysisAction;
import org.wikipediacleaner.gui.swing.action.RemoveAllLinksAction;
import org.wikipediacleaner.gui.swing.action.ReplaceAllLinksAction;
import org.wikipediacleaner.gui.swing.action.TemplatesAnalysisAction;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * A helper class to manage contextual menu.
 */
public class AnalysisPageListMenuCreator extends BasicMenuCreator {

  /**
   * Add item for finding text.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public void addItemFindText(
      JPopupMenu popup, Page page, JTextPane textPane) {
    if ((page != null) && (textPane != null)) {
      addItem(
          popup, null, GT._("Find text"),
          new FindTextAction(page.getTitle(), textPane));
    }
  }

  /**
   * Add item for analyzing templates.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Initial page.
   * @param link Link.
   */
  public void addItemAnalyzeTemplates(
      EnumWikipedia wiki, JPopupMenu popup, Page page, Page link) {
    if ((page != null) && (wiki != null)) {
      addItem(
          popup, null, GT._("Search in templates"),
          new TemplatesAnalysisAction(page, link, wiki));
    }
  }

  /**
   * Add item for removing links.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public void addItemRemoveAllLinks(
      JPopupMenu popup, Page page, MWPane textPane) {
    addItem(
        popup, null, GT._("Remove all links"),
        new RemoveAllLinksAction(textPane, page));
  }

  /**
   * Add submenus for replacing links.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public void addReplaceAllLinks(
      JPopupMenu popup, Page page, MWPane textPane) {
    List<Page> links = page.getLinksWithRedirect();
    if ((links != null) && (links.size() > 0)) {
      JMenu submenuLink = new JMenu(GT._("Link to"));

      int fixedBeginLink = 0;
      int fixedEndLink = 0;
      String title = LinkReplacement.getLastReplacement(page.getTitle());
      if (title != null) {
        fixedBeginLink += addItem(
            submenuLink, null, title,
            new ReplaceAllLinksAction(textPane, page, title));
        fixedBeginLink += addSeparator(submenuLink);
      }

      for (Page p : links) {
        if (p.isRedirect()) {
          JMenu submenu1 = new JMenu(p.getTitle());
          
          Iterator<Page> iter = p.getRedirectIteratorWithPage();
          while (iter.hasNext()) {
            Page pageTmp = iter.next();
            addItem(
                submenu1, pageTmp, null,
                new ReplaceAllLinksAction(textPane, page, pageTmp.getTitle()));
          }
          
          submenuLink.add(submenu1);
        } else {
          addItem(
              submenuLink, p, null,
              new ReplaceAllLinksAction(textPane, page, p.getTitle()));
        }
      }

      title = LinkReplacement.getLastReplacement(page.getTitle());
      if (title != null) {
        fixedEndLink += addSeparator(submenuLink);
        fixedEndLink += addItem(
            submenuLink, null, title,
            new ReplaceAllLinksAction(textPane, page, title));
      }

      addSubmenu(popup, submenuLink, fixedBeginLink, fixedEndLink);
    }
  }

  /**
   * Add item for analyzing missing page.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public void addItemRedLinksAnalysis(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, MWPane textPane) {
    JMenuItem menuItem = new JMenuItem(GT._("Red links analysis"));
    ActionListener action = new RedLinksAnalysisAction(page, textPane, wikipedia);
    menuItem.addActionListener(action);
    menuItem.setEnabled(false); //TODO and use addItem()
    popup.add(menuItem);
  }
}
