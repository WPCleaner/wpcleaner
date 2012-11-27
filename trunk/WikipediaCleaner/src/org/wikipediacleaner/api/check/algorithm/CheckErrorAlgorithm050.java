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

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.check.HtmlCharacters;


/**
 * Algorithm for analyzing error 50 of check wikipedia project.
 * Error 50: en dash or em dash
 */
public class CheckErrorAlgorithm050 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm050() {
    super("en dash or em dash");
    htmlCharacters = new ArrayList<HtmlCharacters>();
    htmlCharacters.add(HtmlCharacters.SYMBOL_EM_DASH);
    htmlCharacters.add(HtmlCharacters.SYMBOL_EN_DASH);
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }
}
