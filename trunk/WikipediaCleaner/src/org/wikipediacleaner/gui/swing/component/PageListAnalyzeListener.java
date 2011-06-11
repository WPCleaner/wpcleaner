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

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.JList;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.AnalysisWindow;
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
      for (Page backLink : basePage.getBackLinksWithRedirects()) {
        if ((backLink != null) &&
            (backLink.isRedirect()) &&
            (Page.areSameTitle(basePage.getTitle(), backLink.getRedirectDestination()))) {
          knownPages.add(backLink);
        }
      }
    }
    AnalysisWindow.createAnalysisWindow(page.getTitle(), knownPages, wikipedia);
  }
}
