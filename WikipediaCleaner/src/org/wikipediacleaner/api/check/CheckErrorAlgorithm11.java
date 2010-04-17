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

import java.util.ArrayList;

import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 11 of check wikipedia project.
 * Error 11: HTML named entities
 */
public class CheckErrorAlgorithm11 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm11() {
    super("HTML named entities");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    result |= simpleTextSearch(page, contents, errors, "&acirc;", "â");
    result |= simpleTextSearch(page, contents, errors, "&agrave;", "à");
    result |= simpleTextSearch(page, contents, errors, "&cap;", "∩");
    result |= simpleTextSearch(page, contents, errors, "&ccedil;", "ç");
    result |= simpleTextSearch(page, contents, errors, "&cup;", "∪");
    result |= simpleTextSearch(page, contents, errors, "&deg;", "°");
    result |= simpleTextSearch(page, contents, errors, "&eacute;", "é");
    result |= simpleTextSearch(page, contents, errors, "&ecirc;", "ê");
    result |= simpleTextSearch(page, contents, errors, "&egrave;", "è");
    result |= simpleTextSearch(page, contents, errors, "&hellip;", "…");
    result |= simpleTextSearch(page, contents, errors, "&iacute;", "í");
    result |= simpleTextSearch(page, contents, errors, "&isin;", "∈");
    result |= simpleTextSearch(page, contents, errors, "&lambda;", "λ");
    result |= simpleTextSearch(page, contents, errors, "&laquo;", "«");
    result |= simpleTextSearch(page, contents, errors, "&larr;", "←");
    result |= simpleTextSearch(page, contents, errors, "&le;", "≤");
    result |= simpleTextSearch(page, contents, errors, "&middot;", "·");
    result |= simpleTextSearch(page, contents, errors, "&minus;", "−");
    result |= simpleTextSearch(page, contents, errors, "&nu;", "ν");
    result |= simpleTextSearch(page, contents, errors, "&quot;", "\"");
    result |= simpleTextSearch(page, contents, errors, "&raquo;", "»");
    result |= simpleTextSearch(page, contents, errors, "&rarr;", "→");
    result |= simpleTextSearch(page, contents, errors, "&sect;", "§");
    result |= simpleTextSearch(page, contents, errors, "&sigma;", "σ");
    result |= simpleTextSearch(page, contents, errors, "&sube;", "⊆");
    result |= simpleTextSearch(page, contents, errors, "&thinsp;", " ");
    return result;
  }
}
