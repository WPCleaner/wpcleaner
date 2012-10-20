/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import org.wikipediacleaner.api.data.Suggestion;


/**
 * Action to activate or deactivate a chapter for suggestions.
 */
public class ActivateChapterAction extends AbstractAction {

  /**
   * Generated.
   */
  private static final long serialVersionUID = -7254919618060842053L;

  /**
   * Chapter to activate or deactivate.
   */
  private final String chapter;

  /**
   * @param name Displayed name.
   * @param active Is it active ?
   * @param chapter Actual chapter name.
   */
  public ActivateChapterAction(String name, boolean active, String chapter) {
    super(name);
    putValue(SELECTED_KEY, active);
    this.chapter = chapter;
  }

  /**
   * Invoked when an action occurs.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    Object selected = getValue(SELECTED_KEY);
    if (selected instanceof Boolean) {
      Suggestion.activateChapter(chapter, ((Boolean) selected).booleanValue());
    }
  }

}
