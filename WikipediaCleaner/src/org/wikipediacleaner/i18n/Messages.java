/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.i18n;

import java.util.Enumeration;
import java.util.ResourceBundle;


/**
 * Base class for managing internationalization.
 */
public class Messages extends ResourceBundle {

  /**
   * Constructor.
   */
  public Messages() {
    super();
  }

  /* (non-Javadoc)
   * @see java.util.ResourceBundle#getKeys()
   */
  @Override
  public Enumeration<String> getKeys() {
    return null;
  }

  /* (non-Javadoc)
   * @see java.util.ResourceBundle#handleGetObject(java.lang.String)
   */
  @Override
  protected Object handleGetObject(String key) {
    if (key == null) {
      return null;
    }
    return key.replaceAll(" \\[X+\\]$", "");
  }
}
