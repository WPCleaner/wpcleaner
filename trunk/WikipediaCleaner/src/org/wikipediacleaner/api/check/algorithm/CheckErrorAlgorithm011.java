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

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 11 of check wikipedia project.
 * Error 11: HTML named entities
 */
public class CheckErrorAlgorithm011 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Replace all"),
  };

  private final static String[][] characters = new String[][] {
    { "&Aacute;", "Á" },
    { "&Agrave;", "À" },
    { "&Aring;" , "Å" },
    { "&aacute;", "á" },
    { "&acirc;" , "â" },
    { "&agrave;", "à" },
    { "&atilde;", "ã" },

    { "&bull;"   , "•" },

    { "&cap;"   , "∩" },
    { "&ccedil;", "ç" },
    { "&copy;"  , "©" },
    { "&cup;"   , "∪" },

    { "&deg;"   , "°" },

    { "&Eacute;", "É" },
    { "&eacute;", "é" },
    { "&ecirc;" , "ê" },
    { "&egrave;", "è" },

    { "&hellip;", "…" },

    { "&iacute;", "í" },
    { "&infin;" , "∞" },
    { "&isin;"  , "∈" },

    { "&lambda;", "λ" },
    { "&laquo;" , "«" },
    { "&larr;"  , "←" },
    { "&le;"    , "≤" },

    { "&middot;", "·" },
    { "&minus;" , "−" },

    { "&ne;"    , "≠" },
    { "&ntilde;", "ñ" },
    { "&nu;"    , "ν" },

    { "&Omega;" , "Ω" },
    { "&omega;" , "ω" },
    { "&ouml;"  , "ö" },

    { "&quot;"  , "\"" },

    { "&raquo;" , "»" },
    { "&rarr;"  , "→" },
    { "&reg;"   , "®" },
    { "&rsquo;" , "’" },

    { "&sect;"  , "§" },
    { "&sigma;" , "σ" },
    { "&sube;"  , "⊆" },

    { "&thinsp;", " " },
    { "&times;" , "×" },

    { "&Uuml;"  , "Ü" },
    { "&uuml;"  , "ü" },
  };

  public CheckErrorAlgorithm011() {
    super("HTML named entities");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments in the page contents.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    for (int i = 0; i < characters.length; i++) {
      result |= simpleTextSearch(page, contents, errors, characters[i][0], characters[i][1]);
      if ((result) && (errors == null)) {
        return true;
      }
    }
    return result;
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, Page page, String contents) {
    String result = contents;
    for (int i = 0; i < characters.length; i++) {
      result = result.replaceAll(characters[i][0], characters[i][1]);
    }
    return result;
  }
}
