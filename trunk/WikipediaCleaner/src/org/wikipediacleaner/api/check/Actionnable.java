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

import java.util.List;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;


/**
 * A generic interface for grouping possible actions.
 */
public interface Actionnable {

  /**
   * @return Action name.
   */
  public String getName();

  /**
   * @return Flag indicating if it is composed action.
   */
  public boolean isCompositeAction();

  /**
   * @param newText New text.
   * @return True if this action can give this new text.
   */
  public boolean isPossibleReplacement(String newText);

  /**
   * @param element Text element.
   * @param textPane Text component.
   * @return Action.
   */
  public Action getAction(Element element, JTextPane textPane);

  /**
   * @return Actions.
   */
  public List<Actionnable> getActions();
}
