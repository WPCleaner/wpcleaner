/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

  @Override
  public int getIconHeight() {
    return SIZE;
  }

  @Override
  public int getIconWidth() {
    return SIZE;
  }

  /* (non-Javadoc)
   * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
   */
  @Override
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
