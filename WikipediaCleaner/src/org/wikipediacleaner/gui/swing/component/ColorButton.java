/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JColorChooser;


/**
 * A button used for choosing a color.
 */
public class ColorButton extends JButton implements ActionListener {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = -4093271062830284750L;

  /**
   * Title for the color chooser dialog.
   */
  private final String title;

  /**
   * Create a color button.
   *
   * @param color Color.
   */
  public ColorButton(Color color, String title) {
    super("X");
    this.title = title;
    setColor(color);
    addActionListener(this);
    setFocusable(false);
    setFocusPainted(false);
    setOpaque(true);
    setRolloverEnabled(false);
  }

  /**
   * @param color Color.
   */
  public void setColor(Color color) {
    setBackground(color);
    setForeground(color);
  }

  /**
   * @return Color.
   */
  public Color getColor() {
    return getBackground();
  }

  /**
   * Choose color.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    Color color = JColorChooser.showDialog(getParent(), title, getColor());
    if (color != null) {
      setColor(color);
    }
  }
}
