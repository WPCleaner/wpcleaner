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

package org.wikipediacleaner.gui.swing.options;

import java.awt.LayoutManager;

import javax.swing.JPanel;


/**
 * Base class for an Options panel.
 */
abstract class OptionsPanel extends JPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = -6581007549837818171L;

  /**
   * Construct an Options panel.
   * 
   * @param layout Layout manager.
   */
  public OptionsPanel(LayoutManager layout) {
    super(layout);
  }

  /**
   * Restore all options to their default values.
   */
  public abstract void defaultValues();

  /**
   * Apply new values to the options.
   */
  public abstract void apply();
}
