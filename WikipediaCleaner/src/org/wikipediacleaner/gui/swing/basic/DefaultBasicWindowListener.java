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

package org.wikipediacleaner.gui.swing.basic;


/**
 * Default basic window creation callbacks.
 */
public abstract class DefaultBasicWindowListener implements BasicWindowListener {

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindowCreation#initializeWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
   */
  public void initializeWindow(
      @SuppressWarnings("unused") BasicWindow window) {
    //
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindowCreation#displayWindow(org.wikipediacleaner.gui.swing.basic.BasicWindow)
   */
  public void displayWindow(
      @SuppressWarnings("unused") BasicWindow window) {
    //
  }
}
