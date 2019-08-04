/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.JList;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageRedirect;
import org.wikipediacleaner.gui.swing.OnePageAnalysisWindow;
import org.wikipediacleaner.gui.swing.OnePageWindow;


/**
 * A analyze listener for MediaWikiPane. 
 */
public class PageListAnalyzeListener extends MouseAdapter {

  private final EnumWikipedia wikipedia;
  private final OnePageWindow pageWindow;

  public PageListAnalyzeListener(EnumWikipedia wikipedia, OnePageWindow pageWindow) {
    this.wikipedia = wikipedia;
    this.pageWindow = pageWindow;
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseClicked(MouseEvent e) {
    if (e.getButton() != MouseEvent.BUTTON1) {
      return;
    }
    if (e.getClickCount() != 2) {
      return;
    }
    if (!(e.getComponent() instanceof JList)) {
      return;
    }
    JList list = (JList) e.getComponent();
    int position = list.locationToIndex(e.getPoint());
    if (position < 0) {
      return;
    }
    Object object = list.getModel().getElementAt(position);
    if (!(object instanceof Page)) {
      return;
    }
    Page page = (Page) object;
    ArrayList<Page> knownPages = null;
    if ((pageWindow != null) && (pageWindow.getPage() != null)) {
      Page basePage = pageWindow.getPage();
      knownPages = new ArrayList<Page>(1);
      knownPages.add(basePage);
      for (Page backLink : basePage.getAllLinksToPage()) {
        PageRedirect redirects = backLink.getRedirects();
        if ((redirects != null) &&
            (redirects.isRedirect()) &&
            (Page.areSameTitle(basePage.getTitle(), redirects.getTitle()))) {
          knownPages.add(backLink);
        }
      }
    }
    OnePageAnalysisWindow.createAnalysisWindow(page.getTitle(), knownPages, wikipedia);
  }
}
