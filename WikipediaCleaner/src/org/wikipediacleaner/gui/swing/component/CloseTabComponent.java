/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.plaf.basic.BasicButtonUI;

import org.wikipediacleaner.i18n.GT;


/**
 * A tab component for a JTabbedPane allowing to close the tab.
 */
public class CloseTabComponent extends JPanel {

  private static final long serialVersionUID = 1L;

  final JTabbedPane pane;

  private final JLabel label;

  private final JButton button;

  public CloseTabComponent(String title, JTabbedPane pane) {
    super(new FlowLayout(FlowLayout.LEFT, 0, 0));
    this.pane = pane;
    setOpaque(false);
    label = new JLabel(title);
    button = new TabButton();

    add(label);
    label.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
    add(button);
    setBorder(BorderFactory.createEmptyBorder(2, 0, 0, 0));
  }

  /* (non-Javadoc)
   * @see javax.swing.JComponent#processMouseEvent(java.awt.event.MouseEvent)
   */
  @Override
  protected void processMouseEvent(MouseEvent e) {
    super.processMouseEvent(e);
  }

  /**
   * Button for tab. 
   */
  private class TabButton extends JButton implements ActionListener {

    private static final long serialVersionUID = 1L;

    public TabButton() {
      int size = 17;
      setPreferredSize(new Dimension(size, size));
      setToolTipText(GT._("Close this tab"));
      setUI(new BasicButtonUI());
      setContentAreaFilled(false);
      setFocusable(false);
      setBorder(BorderFactory.createEtchedBorder());
      setBorderPainted(false);
      addMouseListener(closeMouseListener);
      setRolloverEnabled(true);
      addActionListener(this);            
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
      int i = pane.indexOfTabComponent(CloseTabComponent.this);
      if (i != -1) {
        pane.remove(i);
      }
    }

    /* (non-Javadoc)
     * @see javax.swing.JButton#updateUI()
     */
    @Override
    public void updateUI() {
      //
    }

    /* (non-Javadoc)
     * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
     */
    @Override
    protected void paintComponent(Graphics g) {
      super.paintComponent(g);
      Graphics2D g2 = (Graphics2D) g;
      Stroke stroke = g2.getStroke();
      //shift the image for pressed buttons
      if (!getModel().isPressed()) {
        g2.translate(-1, -1);
      }
      g2.setStroke(new BasicStroke(2));
      g.setColor(Color.BLACK);
      if (getModel().isRollover()) {
        g.setColor(Color.MAGENTA);
      }
      int delta = 6;
      g.drawLine(delta, delta, getWidth() - delta - 1, getHeight() - delta - 1);
      g.drawLine(getWidth() - delta - 1, delta, delta, getHeight() - delta - 1);
      //leave the graphics unchanged
      if (!getModel().isPressed()) {
        g.translate(1, 1);
      }
      g2.setStroke(stroke);
    }
  }

  final static MouseListener closeMouseListener = new MouseAdapter() {
    /* (non-Javadoc)
     * @see java.awt.event.MouseAdapter#mouseEntered(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseEntered(MouseEvent e) {
      Component component = e.getComponent();
      if (component instanceof AbstractButton) {
        AbstractButton button = (AbstractButton) component;
        button.setBorderPainted(true);
      }
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseAdapter#mouseExited(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseExited(MouseEvent e) {
      Component component = e.getComponent();
      if (component instanceof AbstractButton) {
        AbstractButton button = (AbstractButton) component;
        button.setBorderPainted(false);
      }
    }
  };
}
