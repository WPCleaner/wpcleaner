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

import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * An abstract popup menu listener for Page list. 
 */
public abstract class AbstractPageListPopupListener extends MouseAdapter {

  protected Page page;
  protected final EnumWikipedia wikipedia;
  protected final MediaWikiPane textPane;
  protected final BasicWindow window;

  /**
   * @param wikipedia Wikipedia
   * @param textPane Text pane.
   * @param window Window.
   */
  public AbstractPageListPopupListener(
      EnumWikipedia wikipedia,
      MediaWikiPane textPane,
      BasicWindow   window) {
    this.wikipedia = wikipedia;
    this.textPane = textPane;
    this.window = window;
  }

  /**
   * @param page Page.
   */
  public void setPage(Page page) {
    this.page = page;
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseClicked(MouseEvent e) {
    maybeShowPopup(e);
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
   */
  @Override
  public void mousePressed(MouseEvent e) {
    maybeShowPopup(e);
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseReleased(MouseEvent e) {
    maybeShowPopup(e);
  }

  /**
   * Construct and show popup menu if necessary.
   * 
   * @param e Event.
   */
  private void maybeShowPopup(MouseEvent e) {

    // Retrieve information
    if (!e.isPopupTrigger()) {
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
    Page link = (Page) object;

    // Menu name
    JPopupMenu popup = new JPopupMenu();
    JMenuItem menuItem = new JMenuItem(link.getTitle());
    menuItem.setEnabled(false);
    popup.add(menuItem);

    // Create sub menus
    createPopup(popup, link);

    popup.show(e.getComponent(), e.getX(), e.getY());
  }

  /**
   * Fill popup menu.
   * 
   * @param popup Popup menu.
   * @param link Link.
   */
  abstract protected void createPopup(JPopupMenu popup, Page link);
}
