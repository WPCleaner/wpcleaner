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

package org.wikipediacleaner.api.check;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.gui.swing.action.ReplaceTextAction;


/**
 * An action provider for ReplaceTextAction.
 */
class ReplaceTextActionProvider implements ActionProvider {

  private final String newText;

  private final boolean automatic;

  /**
   * @param newText New text.
   */
  ReplaceTextActionProvider(String newText) {
    this(newText, false);
  }

  /**
   * @param newText New text.
   * @param automatic True if the replacement can be done automatically.
   */
  ReplaceTextActionProvider(String newText, boolean automatic) {
    this.newText = newText;
    this.automatic = automatic;
  }

  /**
   * @return New text.
   */
  public String getNewText() {
    return newText;
  }

  /**
   * @return True if the replacement can be done automatically.
   */
  public boolean isAutomatic() {
    return automatic;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.ActionProvider#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  public Action getAction(Element element, JTextPane textPane) {
    return new ReplaceTextAction(newText, element, textPane);
  }

}
