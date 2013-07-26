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

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.check.HtmlCharacters;


/**
 * Algorithm for analyzing error 11 of check wikipedia project.
 * Error 11: HTML named entities
 */
public class CheckErrorAlgorithm011 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm011() {
    super("HTML named entities");
    htmlCharacters = new ArrayList<HtmlCharacters>();
    for (HtmlCharacters htmlCharacter : HtmlCharacters.values()) {
      if (!HtmlCharacters.SYMBOL_DAGGER.equals(htmlCharacter) &&
          !HtmlCharacters.SYMBOL_EM_DASH.equals(htmlCharacter) &&
          !HtmlCharacters.SYMBOL_EN_DASH.equals(htmlCharacter)) {
        htmlCharacters.add(htmlCharacter);
      }
    }
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }
}
