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

package org.wikipediacleaner.api.check;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.CheckLanguageLinkAction;


/**
 * An action provider for CheckLanguageLinkAction.
 */
public class CheckLanguageLinkActionProvider implements ActionProvider {

  private final EnumWikipedia fromWikipedia;
  private final EnumWikipedia toWikipedia;
  private final String title;
  private final String text;

  /**
   * @param from Wiki on which we need to check the language link.
   * @param to Wiki to which we need to check the language link.
   * @param title Article's title.
   * @param text Text of the link.
   */
  public CheckLanguageLinkActionProvider(
      EnumWikipedia from, EnumWikipedia to,
      String title, String text) {
    this.fromWikipedia = from;
    this.toWikipedia = to;
    this.title = title;
    this.text = text;
  }

  /**
   * @param element Text element.
   * @param textPane Text component.
   * @return Action.
   */
  public Action getAction(Element element, JTextPane textPane) {
    return new CheckLanguageLinkAction(
        fromWikipedia, toWikipedia, title, text, element, textPane);
  }

  /**
   * @param newText New text.
   * @return True if this action can give this new text.
   */
  public boolean isPossibleReplacement(String newText) {
    return false;
  }
}
