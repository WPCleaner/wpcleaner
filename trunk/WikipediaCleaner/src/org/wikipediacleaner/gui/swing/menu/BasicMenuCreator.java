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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageAnalysisUtils;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.action.DisambiguationAnalysisAction;
import org.wikipediacleaner.gui.swing.action.FullPageAnalysisAction;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.gui.swing.action.PurgeCacheAction;
import org.wikipediacleaner.gui.swing.action.ReloadLinksAction;
import org.wikipediacleaner.gui.swing.action.RemoveLinkAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * A helper class to manage contextual menu.
 */
public class BasicMenuCreator extends AbstractMenuCreator {

  // ==========================================================================
  // Menu creation
  // ==========================================================================

  /**
   * @param title Possible title for the menu.
   * @return Popup menu.
   */
  public JPopupMenu createPopupMenu(String title) {
    JPopupMenu menu = new JPopupMenu();
    addDisabledText(menu, title);
    return menu;
  }

  // ==========================================================================
  // View pages
  // ==========================================================================

  /**
   * Add submenus for viewing pages.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param showLinks True if internal links should be displayed.
   */
  public void addView(
      EnumWikipedia wiki, JPopupMenu popup,
      Page page, boolean showLinks) {
    if ((wiki == null) || (popup == null) || (page == null)) {
      return;
    }

    List<Page> links = page.getLinksWithRedirect();
    if (Utilities.isDesktopSupported()) {
      if (((links != null) && (links.size() > 0) && showLinks) ||
          page.isRedirect()) {
        int fixedBeginView = 0;
        int fixedEndView = 0;
        int fixedBeginHistory = 0;
        int fixedEndHistory = 0;
        JMenu submenuView = new JMenu(GT._("External Viewer"));
        JMenu submenuHistory = new JMenu(GT._("History"));
        Iterator<Page> iter = page.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          fixedBeginView += addItem(
              submenuView, pageTmp, null, true,
              new PageViewAction(pageTmp.getTitle(), wiki));
          fixedBeginHistory += addItem(
              submenuHistory, pageTmp, null, true,
              new PageViewAction(pageTmp.getTitle(), wiki, "history"));
        }
        if ((links != null) && (links.size() > 0) && showLinks) {
          fixedBeginView += addSeparator(submenuView);
          fixedBeginHistory += addSeparator(submenuHistory);
    
          for (Page p : links) {
            if (p.isRedirect()) {
              JMenu submenuRedirectView = new JMenu(p.getTitle());
              JMenu submenuRedirectHistory = new JMenu(p.getTitle());
              Iterator<Page> itPage = p.getRedirectIteratorWithPage();
              while (itPage.hasNext()) {
                Page redirect = itPage.next();
                addItem(
                    submenuRedirectView, redirect, null, true,
                    new PageViewAction(redirect.getTitle(), wiki));
                addItem(
                    submenuRedirectHistory, redirect, null, true,
                    new PageViewAction(redirect.getTitle(), wiki, "history"));
              }
              submenuView.add(submenuRedirectView);
              submenuHistory.add(submenuRedirectHistory);
            } else {
              addItem(
                  submenuView, p, null, true,
                  new PageViewAction(p.getTitle(), wiki));
              addItem(
                  submenuHistory, p, null, true,
                  new PageViewAction(p.getTitle(), wiki, "history"));
            }
          }

          fixedEndView += addSeparator(submenuView);
          fixedEndHistory += addSeparator(submenuHistory);

          iter = page.getRedirectIteratorWithPage();
          while (iter.hasNext()) {
            Page pageTmp = iter.next();
            fixedEndView += addItem(
                submenuView, pageTmp, null, true,
                new PageViewAction(pageTmp.getTitle(), wiki));
            fixedEndHistory += addItem(
                submenuHistory, pageTmp, null, true,
                new PageViewAction(pageTmp.getTitle(), wiki, "history"));
          }
        }
        addSubmenu(popup, submenuView, fixedBeginView, fixedEndView);
        addSubmenu(popup, submenuHistory, fixedBeginHistory, fixedEndHistory);
      } else {
        String title = null;
        if ((page.getNamespace() != null) &&
            (page.getNamespace().intValue() == Namespace.TEMPLATE)) {
          title = GT._("Template External Viewer");
        } else {
          title = GT._("External Viewer");
        }
        addItem(
            popup, page, title, true,
            new PageViewAction(page.getTitle(), wiki));
        if ((page.getNamespace() != null) &&
            (page.getNamespace().intValue() == Namespace.TEMPLATE)) {
          title = GT._("Template history");
        } else {
          title = GT._("History");
        }
        addItem(
            popup, page, title, true,
            new PageViewAction(page.getTitle(), wiki, "history"));
      }
    }
  }

  // ==========================================================================
  // Analyze pages
  // ==========================================================================

  /**
   * Add submenus for analyzing pages.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   */
  public void addAnalyze(
      EnumWikipedia wiki, JPopupMenu popup, Page page) {
    if ((wiki == null) || (popup == null) || (page ==  null)) {
      return;
    }
    JMenuItem menuItem = null;
    ActionListener action = null;
    List<Page> links = page.getLinksWithRedirect();
    if (((links != null) && (links.size() > 0)) || page.isRedirect()) {
      int fixedBegin = 0;
      int fixedEnd = 0;
      JMenu submenuAnalyze = new JMenu(GT._("Analyze"));
      Iterator<Page> iter = page.getRedirectIteratorWithPage();
      while (iter.hasNext()) {
        Page pageTmp = iter.next();
        menuItem = new JMenuItem(pageTmp.getTitle());
        updateFont(menuItem, pageTmp);
        action = new FullPageAnalysisAction(pageTmp.getTitle(), wiki);
        menuItem.addActionListener(action);
        submenuAnalyze.add(menuItem);
        fixedBegin++;
      }
      if ((links != null) && (links.size() > 0)) {
        fixedBegin += addSeparator(submenuAnalyze);
  
        for (Page p : links) {
          menuItem = new JMenuItem(p.getTitle());
          updateFont(menuItem, p);
          action = new FullPageAnalysisAction(p.getTitle(), wiki);
          menuItem.addActionListener(action);
          submenuAnalyze.add(menuItem);
        }

        fixedEnd += addSeparator(submenuAnalyze);

        iter = page.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          action = new FullPageAnalysisAction(pageTmp.getTitle(), wiki);
          menuItem.addActionListener(action);
          submenuAnalyze.add(menuItem);
          fixedEnd++;
        }
      }
      addSubmenu(popup, submenuAnalyze, fixedBegin, fixedEnd);
    } else {
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.TEMPLATE)) {
        menuItem = new JMenuItem(GT._("Analyze template"));
      } else {
        menuItem = new JMenuItem(GT._("Analyze page"));
      }
      action = new FullPageAnalysisAction(page.getTitle(), wiki);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  // ==========================================================================
  // Analyze disambiguation pages
  // ==========================================================================

  /**
   * Add submenus for disambiguation.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   */
  public void addDisambiguation(
      EnumWikipedia wiki, JPopupMenu popup, Page page) {
    if ((wiki == null) || (popup == null) || (page == null)) {
      return;
    }
    if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
      if (!page.isRedirect()) {
        JMenuItem menuItem = new JMenuItem(GT._("Disambiguation analysis"));
        ActionListener action = new DisambiguationAnalysisAction(page.getTitle(), wiki);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      } else {
        JMenu submenuView = new JMenu(GT._("Disambiguation analysis"));
        Iterator<Page> iter = page.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          JMenuItem menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          ActionListener action = new DisambiguationAnalysisAction(pageTmp.getTitle(), wiki);
          menuItem.addActionListener(action);
          submenuView.add(menuItem);
        }
        popup.add(submenuView);
      }
    }
  }

  // ==========================================================================
  // Show current chapter
  // ==========================================================================

  /**
   * Add sub menus for showing current chapter organization.
   * 
   * @param popup Pop up menu.
   * @param position Current position in text.
   * @param pageAnalysis Page analysis.
   */
  public void addCurrentChapter(
      JPopupMenu popup,
      int position, PageAnalysis pageAnalysis) {
    if ((popup == null) || (pageAnalysis == null)) {
      return;
    }
    Collection<PageElementTitle> chapters =
        PageAnalysisUtils.getCurrentTitles(pageAnalysis, position);
    if ((chapters != null) && !chapters.isEmpty()) {
      JMenu submenu = new JMenu(GT._("Current chapter"));
      for (PageElementTitle chapter : chapters) {
        addDisabledText(submenu, chapter.toString());
      }
      addSubmenu(popup, submenu, 0, 0);
    }
  }

  // ==========================================================================
  // Basic actions
  // ==========================================================================

  /**
   * Add item for removing link.
   * 
   * @param popup Popup menu.
   * @param text Text.
   * @param textPane Text pane.
   * @param startOffset Start offset.
   * @param endOffset End offset.
   */
  public void addItemRemoveLink(
      JPopupMenu popup, String text,
      MWPane textPane, int startOffset, int endOffset) {
    if (text != null) {
      addItem(
          popup, null, GT._("Remove link"), true,
          new RemoveLinkAction(text, textPane, startOffset, endOffset));
    }
  }

  /**
   * Add item for viewing page.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param url URL.
   */
  public void addItemView(
      EnumWikipedia wiki, JPopupMenu popup, String url) {
    addItemView(wiki, popup, url, GT._("External Viewer"));
  }

  /**
   * Add item for viewing page.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param url URL.
   * @param label Menu label.
   */
  public void addItemView(
      EnumWikipedia wiki, JPopupMenu popup, String url, String label) {
    if ((url != null) && (Utilities.isDesktopSupported())) {
      addItem(popup, null, label, true, new PageViewAction(url, wiki));
    }
  }

  /**
   * Add item for purging page cache. 
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param window Window.
   */
  public void addItemPurgeCache(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, BasicWindow window) {
    if (page != null) {
      addItem(
          popup, null, GT._("Purge cache"), true,
          new PurgeCacheAction(wikipedia, page, window));
    }
  }

  /**
   * Add item for reloading links. 
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param window Window.
   */
  public void addItemReloadLinks(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, BasicWindow window) {
    if ((wikipedia == null) || (popup == null) || (window == null)) {
      return;
    }
    addItem(
        popup, null, GT._("Reload links"), true,
        new ReloadLinksAction(wikipedia, page, window));
  }
}
