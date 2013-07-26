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

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="http://ar.wikipedia.org/w/index.php">Arabic wikipedia</a>.
 */
public final class WikipediaAr extends AbstractWikipediaSettings {

  /**
   * Constructor.
   */
  public WikipediaAr() {
    super("ar", "Arabic Wikipedia");
  }

  /**
   * @return Component orientation.
   */
  @Override
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.RIGHT_TO_LEFT;
  }
}
