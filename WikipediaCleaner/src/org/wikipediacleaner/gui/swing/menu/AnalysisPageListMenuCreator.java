/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.menu;

import java.util.Iterator;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinkReplacement;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.action.FindTextAction;
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
          popup, null, GT._T("Find text"), true,
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
          popup, null, GT._T("Search in templates"), true,
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
        popup, null, GT._T("Remove all links"), true,
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
      JMenu submenuLink = new JMenu(GT._T("Link to"));

      int fixedBeginLink = 0;
      int fixedEndLink = 0;
      String title = LinkReplacement.getLastReplacement(page.getTitle());
      if (title != null) {
        fixedBeginLink += addItem(
            submenuLink, null, title, true,
            new ReplaceAllLinksAction(textPane, page, title, null));
        fixedBeginLink += addSeparator(submenuLink);
      }

      for (Page p : links) {
        if (p.isRedirect()) {
          JMenu submenu1 = new JMenu(p.getTitle());
          
          Iterator<Page> iter = p.getRedirectIteratorWithPage();
          while (iter.hasNext()) {
            Page pageTmp = iter.next();
            addItem(
                submenu1, pageTmp, null, true,
                new ReplaceAllLinksAction(textPane, page, pageTmp.getTitle(), null));
          }
          
          submenuLink.add(submenu1);
        } else {
          addItem(
              submenuLink, p, null, true,
              new ReplaceAllLinksAction(textPane, page, p.getTitle(), null));
        }
      }

      title = LinkReplacement.getLastReplacement(page.getTitle());
      if (title != null) {
        fixedEndLink += addSeparator(submenuLink);
        fixedEndLink += addItem(
            submenuLink, null, title, true,
            new ReplaceAllLinksAction(textPane, page, title, null));
      }

      addSubmenu(popup, submenuLink, fixedBeginLink, fixedEndLink);
    }
  }
}
