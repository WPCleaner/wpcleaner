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

import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractButton;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.CheckErrorGlobalFixAction;
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
    JPopupMenu popup = new JPopupMenu();
    JMenuItem menuItem = new JMenuItem(GT._("Error n°{0}", algorithm.getErrorNumberString()));
    menuItem.setEnabled(false);
    popup.add(menuItem);

    // Global fixes
    String[] fixes = algorithm.getGlobalFixes();
    if ((fixes != null) && (fixes.length > 0)) {
      popup.add(new JSeparator());
      for (int i = 0; i < fixes.length; i++) {
        menuItem = new JMenuItem(fixes[i]);
        ActionListener action = new CheckErrorGlobalFixAction(
            algorithm, fixes[i], error.getPage(), textComponent, button);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      }
    }

    // Create sub menus
    popup.add(new JSeparator());
    MenuCreator.addViewToMenu(wikipedia, popup, algorithm.getLink(), GT._("Detail"));
    String toolserverUrl =
      "http://toolserver.org/~sk/cgi-bin/checkwiki/checkwiki.cgi" +
      "?id=" + algorithm.getErrorNumberString() +
      "&project=" + wikipedia.getSettings().getCodeCheckWiki() +
      "&view=only";
    MenuCreator.addViewToMenu(null, popup, toolserverUrl, GT._("List on toolserver"));

    popup.show(e.getComponent(), e.getX(), e.getY());
  }
}
