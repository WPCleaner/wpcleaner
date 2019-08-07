/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.menu;

import java.awt.Component;
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
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.action.ActionCheckTemplate;
import org.wikipediacleaner.gui.swing.action.ActionCopyText;
import org.wikipediacleaner.gui.swing.action.ActionDisambiguationAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionPasteText;
import org.wikipediacleaner.gui.swing.action.PurgeCacheAction;
import org.wikipediacleaner.gui.swing.action.ReloadLinksAction;
import org.wikipediacleaner.gui.swing.action.RemoveLinkAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


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

    List<Page> links = page.getRedirects().getLinks();
    if (Utilities.isDesktopSupported()) {
      if (((links != null) && (links.size() > 0) && showLinks) ||
          page.getRedirects().isRedirect()) {
        int fixedBeginView = 0;
        int fixedEndView = 0;
        int fixedBeginHistory = 0;
        int fixedEndHistory = 0;
        JMenu submenuView = new JMenu(GT._T("External Viewer"));
        JMenu submenuHistory = new JMenu(GT._T("History"));
        Iterator<Page> iter = page.getRedirects().getIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          fixedBeginView += addItem(
              submenuView, pageTmp, null, true,
              new ActionExternalViewer(wiki, pageTmp.getTitle()));
          fixedBeginHistory += addItem(
              submenuHistory, pageTmp, null, true,
              new ActionExternalViewer(
                  wiki, pageTmp.getTitle(), ActionExternalViewer.ACTION_HISTORY));
        }
        if ((links != null) && (links.size() > 0) && showLinks) {
          fixedBeginView += addSeparator(submenuView);
          fixedBeginHistory += addSeparator(submenuHistory);
    
          for (Page p : links) {
            if (p.getRedirects().isRedirect()) {
              JMenu submenuRedirectView = new JMenu(p.getTitle());
              JMenu submenuRedirectHistory = new JMenu(p.getTitle());
              Iterator<Page> itPage = p.getRedirects().getIteratorWithPage();
              while (itPage.hasNext()) {
                Page redirect = itPage.next();
                addItem(
                    submenuRedirectView, redirect, null, true,
                    new ActionExternalViewer(wiki, redirect.getTitle()));
                addItem(
                    submenuRedirectHistory, redirect, null, true,
                    new ActionExternalViewer(
                        wiki, redirect.getTitle(), ActionExternalViewer.ACTION_HISTORY));
              }
              submenuView.add(submenuRedirectView);
              submenuHistory.add(submenuRedirectHistory);
            } else {
              addItem(
                  submenuView, p, null, true,
                  new ActionExternalViewer(wiki, p.getTitle()));
              addItem(
                  submenuHistory, p, null, true,
                  new ActionExternalViewer(
                      wiki, p.getTitle(), ActionExternalViewer.ACTION_HISTORY));
            }
          }

          fixedEndView += addSeparator(submenuView);
          fixedEndHistory += addSeparator(submenuHistory);

          iter = page.getRedirects().getIteratorWithPage();
          while (iter.hasNext()) {
            Page pageTmp = iter.next();
            fixedEndView += addItem(
                submenuView, pageTmp, null, true,
                new ActionExternalViewer(wiki, pageTmp.getTitle()));
            fixedEndHistory += addItem(
                submenuHistory, pageTmp, null, true,
                new ActionExternalViewer(
                    wiki, pageTmp.getTitle(), ActionExternalViewer.ACTION_HISTORY));
          }
        }
        addSubmenu(popup, submenuView, fixedBeginView, fixedEndView);
        addSubmenu(popup, submenuHistory, fixedBeginHistory, fixedEndHistory);
      } else {
        String title = null;
        if ((page.getNamespace() != null) &&
            (page.getNamespace().intValue() == Namespace.TEMPLATE)) {
          title = GT._T("Template External Viewer");
        } else {
          title = GT._T("External Viewer");
        }
        addItem(
            popup, page, title, true,
            new ActionExternalViewer(wiki, page.getTitle()));
        if ((page.getNamespace() != null) &&
            (page.getNamespace().intValue() == Namespace.TEMPLATE)) {
          title = GT._T("Template history");
        } else {
          title = GT._T("History");
        }
        addItem(
            popup, page, title, true,
            new ActionExternalViewer(
                wiki, page.getTitle(), ActionExternalViewer.ACTION_HISTORY));
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
    List<Page> links = page.getRedirects().getLinks();
    if (((links != null) && (links.size() > 0)) || page.getRedirects().isRedirect()) {
      int fixedBegin = 0;
      int fixedEnd = 0;
      JMenu submenuAnalyze = new JMenu(GT._T("Analyze"));
      Iterator<Page> iter = page.getRedirects().getIteratorWithPage();
      while (iter.hasNext()) {
        Page pageTmp = iter.next();
        menuItem = new JMenuItem(pageTmp.getTitle());
        updateFont(menuItem, pageTmp);
        action = new ActionFullAnalysis(wiki, pageTmp.getTitle());
        menuItem.addActionListener(action);
        submenuAnalyze.add(menuItem);
        fixedBegin++;
      }
      if ((links != null) && (links.size() > 0)) {
        fixedBegin += addSeparator(submenuAnalyze);
  
        for (Page p : links) {
          menuItem = new JMenuItem(p.getTitle());
          updateFont(menuItem, p);
          action = new ActionFullAnalysis(wiki, p.getTitle());
          menuItem.addActionListener(action);
          submenuAnalyze.add(menuItem);
        }

        fixedEnd += addSeparator(submenuAnalyze);

        iter = page.getRedirects().getIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          action = new ActionFullAnalysis(wiki, pageTmp.getTitle());
          menuItem.addActionListener(action);
          submenuAnalyze.add(menuItem);
          fixedEnd++;
        }
      }
      addSubmenu(popup, submenuAnalyze, fixedBegin, fixedEnd);
    } else {
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.TEMPLATE)) {
        menuItem = new JMenuItem(GT._T("Analyze template"));
      } else {
        menuItem = new JMenuItem(GT._T("Analyze page"));
      }
      action = new ActionFullAnalysis(wiki, page.getTitle());
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
      if (!page.getRedirects().isRedirect()) {
        JMenuItem menuItem = new JMenuItem(GT._T("Disambiguation analysis"));
        ActionListener action = new ActionDisambiguationAnalysis(wiki, page.getTitle());
        menuItem.addActionListener(action);
        popup.add(menuItem);
      } else {
        JMenu submenuView = new JMenu(GT._T("Disambiguation analysis"));
        Iterator<Page> iter = page.getRedirects().getIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          JMenuItem menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          ActionListener action = new ActionDisambiguationAnalysis(wiki, pageTmp.getTitle());
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
      JMenu submenu = new JMenu(GT._T("Current chapter"));
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
   * Add items for copying/pasting text.
   * 
   * @param popup Popup menu.
   * @param textPane Text pane.
   */
  public void addItemCopyPaste(
      JPopupMenu popup, MWPane textPane) {
    if ((textPane != null) &&
        (Configuration.getConfiguration().getBoolean(null, ConfigurationValueBoolean.COPY_PASTE))) {
      popup.add(ActionCopyText.createMenuItem(textPane));
      JMenuItem menuItemPaste = ActionPasteText.createMenuItem(textPane);
      if (menuItemPaste != null) {
        popup.add(menuItemPaste);
      }
    }
  }

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
          popup, null, GT._T("Remove link"), true,
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
    addItemView(wiki, popup, url, GT._T("External Viewer"));
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
      addItem(popup, null, label, true, new ActionExternalViewer(wiki, url));
    }
  }

  /**
   * Add item for viewing page.
   * 
   * @param wiki Wiki.
   * @param menu Menu.
   * @param url URL.
   * @param label Menu label.
   */
  public void addItemView(
      EnumWikipedia wiki, JMenu menu, String url, String label) {
    if ((url != null) && (Utilities.isDesktopSupported())) {
      addItem(menu, null, label, true, new ActionExternalViewer(wiki, url));
    }
  }

  /**
   * Add item for purging page cache. 
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param window Window.
   */
  public void addItemPurgeCache(
      EnumWikipedia wiki, JPopupMenu popup, Page page, BasicWindow window) {
    if (page != null) {
      addItem(
          popup, null, GT._T("Purge cache"), true,
          new PurgeCacheAction(wiki, page, window));
    }
  }

  /**
   * Add item for reloading links. 
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param window Window.
   */
  public void addItemReloadLinks(
      EnumWikipedia wiki, JPopupMenu popup, Page page, BasicWindow window) {
    if ((wiki == null) || (popup == null) || (window == null)) {
      return;
    }
    addItem(
        popup, null, GT._T("Reload links"), true,
        new ReloadLinksAction(wiki, page, window));
  }

  // ==========================================================================
  // Check article
  // ==========================================================================

  /**
   * Add menu for checking template.
   * 
   * @param wiki Wiki.
   * @param parent Parent component.
   * @param popup Popup menu.
   * @param template Template.
   */
  public void addCheckTemplate(
      EnumWikipedia wiki, Component parent, JPopupMenu popup,
      PageElementTemplate template) {
    if ((popup == null) || (template == null)) {
      return;
    }
    popup.add(ActionCheckTemplate.createMenuItem(parent, wiki, template));
  }
}
