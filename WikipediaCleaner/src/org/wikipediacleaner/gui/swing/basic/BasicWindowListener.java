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
 * An interface used to listen to BasicWindow events.
 */
public interface BasicWindowListener {

  /**
   * Called just after BasicWindow constructor has been called.
   * 
   * @param window BasicWindow.
   */
  public void initializeWindow(BasicWindow window);

  /**
   * Called just after BasicWindow has been displayed.
   * 
   * @param window BasicWindow.
   */
  public void displayWindow(BasicWindow window);
}
