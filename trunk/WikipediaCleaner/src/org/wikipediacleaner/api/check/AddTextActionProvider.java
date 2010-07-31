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
  private final String defaultValue;

  /**
   * @param prefix Prefix.
   * @param suffix Suffix.
   * @param url Optional URL to retrieve title.
   * @param question Question asked to the user.
   */
  public AddTextActionProvider(String prefix, String suffix, String url, String question) {
    this(prefix, suffix, url, question, "");
  }

  /**
   * @param prefix Prefix.
   * @param suffix Suffix.
   * @param url Optional URL to retrieve title.
   * @param question Question asked to the user.
   * @param defaultValue Value used by default.
   */
  public AddTextActionProvider(String prefix, String suffix, String url, String question, String defaultValue) {
    this.prefix = prefix;
    this.suffix = suffix;
    this.url = url;
    this.question = question;
    this.defaultValue = defaultValue;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.ActionProvider#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  public Action getAction(Element element, JTextPane textPane) {
    return new AddTextAction(prefix, suffix, url, question, defaultValue, element, textPane);
  }

}
