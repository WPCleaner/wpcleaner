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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
  private final List<String> chapters;

  /**
   * True if the suggestions should be activated.
   */
  private final boolean activate;

  /**
   * @param name Displayed name.
   * @param active Is it active ?
   * @param chapter Actual chapter name.
   */
  public ActivateChapterAction(String name, boolean active, String chapter) {
    super(name);
    putValue(SELECTED_KEY, active);
    this.chapters = Collections.singletonList(chapter);
    this.activate = !active;
  }

  /**
   * @param name Displayed name.
   * @param activate Should we activate the chapters ?
   * @param page Page.
   * @param chapters Actual chapters name.
   */
  public ActivateChapterAction(String name, boolean activate, String page, List<String> chapters) {
    super(name);
    this.chapters = new ArrayList<String>(chapters.size());
    for (String chapter : chapters) {
      this.chapters.add(page + "#" + chapter);
    }
    this.activate = activate;
  }

  /**
   * Invoked when an action occurs.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    for (String chapter : chapters) {
      Suggestion.activateChapter(chapter, activate);
    }
  }
}
