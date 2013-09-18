/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractButton;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.CheckWiki;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.CheckErrorGlobalFixAction;
import org.wikipediacleaner.gui.swing.menu.BasicMenuCreator;
import org.wikipediacleaner.i18n.GT;


/**
 * A popup menu listener for CheckErrorPage list. 
 */
public class CheckErrorPageListPopupListener extends MouseAdapter {

  private final EnumWikipedia wikipedia;
  private final MWPane textComponent;
  private final AbstractButton button;

  public CheckErrorPageListPopupListener(
      EnumWikipedia wikipedia,
      MWPane textComponent,
      AbstractButton button) {
    this.wikipedia = wikipedia;
    this.textComponent = textComponent;
    this.button = button;
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
    if (!(object instanceof CheckErrorPage)) {
      return;
    }
    CheckErrorPage error = (CheckErrorPage) object;
    CheckErrorAlgorithm algorithm = error.getAlgorithm();

    // Menu name
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(GT._("Error n°{0}", algorithm.getErrorNumberString()));

    // Global fixes
    JMenuItem menuItem = null;
    String[] fixes = algorithm.getGlobalFixes();
    if ((fixes != null) && (fixes.length > 0)) {
      menu.addSeparator(popup);
      for (int i = 0; i < fixes.length; i++) {
        menuItem = new JMenuItem(fixes[i]);
        ActionListener action = new CheckErrorGlobalFixAction(
            algorithm, fixes[i], error.getPage(), textComponent, button);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      }
    }

    // Create sub menus
    menu.addSeparator(popup);
    menu.addItemView(wikipedia, popup, algorithm.getLink(), GT._("Detail"));
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    String toolserverUrl = checkWiki.getUrlDescription(wikipedia, algorithm);
    menu.addItemView(null, popup, toolserverUrl, GT._("List on toolserver"));
    menu.addItemView(wikipedia, popup, algorithm.getWhiteListPageName(), GT._("View or edit white list"));

    popup.show(e.getComponent(), e.getX(), e.getY());
  }
}