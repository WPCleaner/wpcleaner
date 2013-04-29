/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.bot;

import java.awt.LayoutManager;

import javax.swing.JPanel;

import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * Base class for a Bot panel.
 */
abstract class BotToolsPanel extends JPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -6986151664537866639L;

  protected final BasicWindow window;

  /**
   * Construct a Bot panel.
   * 
   * @param layout Layout manager.
   */
  public BotToolsPanel(BasicWindow window, LayoutManager layout) {
    super(layout);
    this.window = window;
  }

  /**
   * Update components state.
   */
  protected void updateComponentState() {
    // Nothing done by default
  }
}
