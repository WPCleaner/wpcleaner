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

import org.wikipediacleaner.gui.swing.action.AddTextAction;


/**
 * An action provider for adding .
 */
public class AddTextActionProvider implements ActionProvider {

  private final String prefix;
  private final String suffix;
  private final String url;
  private final String question;
  private final String[] possibleValues;
  private final boolean onlyList;
  private final String defaultValue;
  private final String unauthorizedCharacters;

  /**
   * @param prefix Prefix.
   * @param suffix Suffix.
   * @param url Optional URL to retrieve title.
   * @param question Question asked to the user.
   * @param unauthorizedCharacters Unauthorized characters.
   */
  public AddTextActionProvider(
      String prefix, String suffix,
      String url, String question,
      String unauthorizedCharacters) {
    this(prefix, suffix, url, question, "", unauthorizedCharacters);
  }

  /**
   * @param prefix Prefix.
   * @param suffix Suffix.
   * @param url Optional URL to retrieve title.
   * @param question Question asked to the user.
   * @param defaultValue Value used by default.
   * @param unauthorizedCharacters Unauthorized characters.
   */
  public AddTextActionProvider(
      String prefix, String suffix, String url,
      String question, String defaultValue,
      String unauthorizedCharacters) {
    this(
        prefix, suffix, url, question,
        null, false, defaultValue, unauthorizedCharacters);
  }

  /**
   * @param prefix Prefix.
   * @param suffix Suffix.
   * @param url Optional URL to retrieve title.
   * @param question Question asked to the user.
   * @param defaultValue Value used by default.
   * @param unauthorizedCharacters Unauthorized characters.
   */
  public AddTextActionProvider(
      String prefix, String suffix, String url,
      String question,
      String[] possibleValues, boolean onlyList, String defaultValue,
      String unauthorizedCharacters) {
    this.prefix = prefix;
    this.suffix = suffix;
    this.url = url;
    this.question = question;
    this.possibleValues = possibleValues;
    this.onlyList = onlyList;
    this.defaultValue = defaultValue;
    this.unauthorizedCharacters = unauthorizedCharacters;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.ActionProvider#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  public Action getAction(Element element, JTextPane textPane) {
    return new AddTextAction(
        prefix, suffix, url, question,
        possibleValues, onlyList, defaultValue,
        unauthorizedCharacters,
        element, textPane);
  }

}
