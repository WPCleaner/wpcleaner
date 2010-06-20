/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.Icon;
import javax.swing.JTabbedPane;


/**
 * A close icon for JTabbedPane. 
 */
public class JTabbedPaneCloseIcon implements Icon {

  private final static int SIZE = 10;
  transient Rectangle position = null; 

  /**
   * @param pane Pane component.
   * @param component Page.
   */
  public JTabbedPaneCloseIcon(final JTabbedPane pane, final Component component) {
    MouseAdapter adapter = new MouseAdapter() {
      @Override
      public void mouseClicked(MouseEvent e) {
        if (!e.isConsumed() &&
            (position != null) &&
            (e.getButton() == MouseEvent.BUTTON1)) {
          if ((e.getClickCount() == 1) &&
              (position.contains(e.getX(), e.getY()))) {
            for (int i = pane.getComponentCount(); i > 0; i--) {
              if (pane.getComponent(i - 1) == component) {
                pane.remove(i - 1);
                pane.removeMouseListener(this);
                e.consume();
                return;
              }
            }
          }
        }
      }
    };
    pane.addMouseListener(adapter);
  }

  public int getIconHeight() {
    return SIZE;
  }

  public int getIconWidth() {
    return SIZE;
  }

  /* (non-Javadoc)
   * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
   */
  public void paintIcon(@SuppressWarnings("unused") Component c, Graphics g, int x, int y) {
    if (g instanceof Graphics2D) {
      Graphics2D g2 = (Graphics2D) g.create();
      g2.draw3DRect(x, y, getIconWidth() - 1, getIconHeight() - 1, false);
      g2.drawLine(x + 2, y + 2, x + getIconWidth() - 3, y + getIconHeight() - 3);
      g2.drawLine(x + 2, y + getIconHeight() - 3, x + getIconWidth() - 3, y + 2);
      g2.dispose();
    }
    position = new Rectangle(x, y, getIconWidth(), getIconHeight());
  }

}
