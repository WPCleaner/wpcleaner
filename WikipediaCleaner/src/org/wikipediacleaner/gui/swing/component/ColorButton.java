/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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
    super("    ");
    this.title = title;
    setColor(color);
    addActionListener(this);
    setFocusable(false);
    setFocusPainted(false);
    setRolloverEnabled(false);
  }

  /**
   * @param color Color.
   */
  public void setColor(Color color) {
    setBackground(color);
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
  public void actionPerformed(ActionEvent e) {
    Color color = JColorChooser.showDialog(getParent(), title, getColor());
    if (color != null) {
      setColor(color);
    }
  }
}
