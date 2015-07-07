/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * An abstract popup menu listener for Page list. 
 */
public abstract class AbstractPageListPopupListener extends AbstractPopupListener implements PopupMenuListener {

  protected Page page;
  protected final EnumWikipedia wikipedia;
  protected final MWPane textPane;
  private final JList list;
  protected final BasicWindow window;

  /**
   * @param wiki Wiki
   * @param textPane Text pane.
   * @param list List.
   * @param window Window.
   */
  public AbstractPageListPopupListener(
      EnumWikipedia wiki,
      MWPane textPane, JList list,
      BasicWindow   window) {
    this.wikipedia = wiki;
    this.textPane = textPane;
    this.list = list;
    this.window = window;
  }

  /**
   * @param page Page.
   */
  public void setPage(Page page) {
    this.page = page;
  }

  /**
   * Show popup menu in response to a mouse event.
   * 
   * @param e Event.
   */
  @Override
  protected void showPopup(MouseEvent e) {

    // Retrieve information
    if (!(e.getComponent() instanceof JList)) {
      return;
    }
    JList tmpList = (JList) e.getComponent();
    int position = tmpList.locationToIndex(e.getPoint());
    if (position < 0) {
      return;
    }
    Object object = tmpList.getModel().getElementAt(position);
    if (!(object instanceof Page)) {
      return;
    }
    Page link = (Page) object;
    showPopup(tmpList, link, e.getX(), e.getY());
  }

  /**
   * Show popup menu in response to a key event.
   * 
   * @param e Event.
   */
  @Override
  protected void showPopup(KeyEvent e) {

    // Retrieve information
    if (!(e.getComponent() instanceof JList)) {
      return;
    }
    JList tmpList = (JList) e.getComponent();
    int position = tmpList.getSelectedIndex();
    if (position < 0) {
      return;
    }
    Object object = tmpList.getModel().getElementAt(position);
    if (!(object instanceof Page)) {
      return;
    }
    Page link = (Page) object;
    Rectangle rect = tmpList.getCellBounds(position, position);
    showPopup(tmpList, link, (int) rect.getMinX(), (int) rect.getMaxY());
  }

  /**
   * Construct and show popup menu.
   * 
   * @param component Component.
   * @param link Selected page.
   * @param x Position.
   * @param y Position.
   */
  private void showPopup(Component component, Page link, int x, int y) {

    // Menu name
    JPopupMenu popup = new JPopupMenu();
    JMenuItem menuItem = new JMenuItem(link.getTitle());
    menuItem.setEnabled(false);
    popup.add(menuItem);

    // Create sub menus
    createPopup(popup, link);

    popup.show(component, x, y);
    popup.addPopupMenuListener(this);
  }

  /**
   * Fill popup menu.
   * 
   * @param popup Popup menu.
   * @param link Link.
   */
  abstract protected void createPopup(JPopupMenu popup, Page link);

  // ==========================================================================
  // PopupMenuListener methods
  // ==========================================================================


  /**
   *  This method is called before the popup menu becomes visible
   *  
   *  @param e Event.
   */
  @Override
  public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
    // Nothing to do
  }

  /**
   * This method is called before the popup menu becomes invisible
   * Note that a JPopupMenu can become invisible any time
   * 
   * @param e Event.
   */
  @Override
  public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
    if (list != null) {
      list.repaint();
    }
  }

  /**
   * This method is called when the popup menu is canceled
   * 
   * @param e Event.
   */
  @Override
  public void popupMenuCanceled(PopupMenuEvent e) {
    // Nothing to do
  }
}
