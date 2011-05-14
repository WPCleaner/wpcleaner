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
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 37 of check wikipedia project.
 * Error 37: Title with special letters and no DEFAULTSORT
 */
public class CheckErrorAlgorithm037 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Add DEFAULTSORT"),
  };

  public CheckErrorAlgorithm037() {
    super("Title with special letters and no DEFAULTSORT");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Analyzing title to find special characters
    String title = pageAnalysis.getPage().getTitle();
    boolean characterFound = false;
    String unknownCharacters = "";
    String text = "";
    int currentPos = 0;
    while (currentPos < title.length()) {
      boolean error = false;
      char character = title.charAt(currentPos);
      if (!SpecialCharacters.isAuthorized(character, pageAnalysis.getWikipedia())) {
        if (currentPos < 3) { // TODO : Parameter
          characterFound = true;
        }
        error = true;
      }
      if (error) {
        String newCharacter = SpecialCharacters.proposeReplacement(character);
        if (!Character.toString(character).equals(newCharacter)) {
          //
        } else {
          unknownCharacters += character;
        }
        text += newCharacter;
      } else {
        text += character;
      }
      currentPos++;
    }
    if (!characterFound) {
      return false;
    }

    // Searching a DEFAULTSORT tag
    String contents = pageAnalysis.getContents();
    PageElementDefaultsort tag = PageContents.findNextDefaultsort(
        pageAnalysis.getPage(), contents, 0);
    if (tag != null) {
      return false;
    }

    // Searching for Categories without a sort key
    boolean categoriesWithoutSort = false;
    int currentIndex = 0;
    while (currentIndex < contents.length()) {
      PageElementCategory category = pageAnalysis.getNextCategory(currentIndex);
      if (category != null) {
        currentIndex = category.getEndIndex();
        if ((category.getSort() == null) ||
            (category.getSort().trim().length() == 0)) {
          categoriesWithoutSort = true;
        }
      } else {
        currentIndex = contents.length();
      }
    }
    if (!categoriesWithoutSort) {
      return false;
    }

    // Reporting error
    return true;
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
    int index = contents.length();
    PageAnalysis pageAnalysis = new PageAnalysis(page, contents);
    PageElementCategory category = pageAnalysis.getNextCategory(0);
    if (category != null) {
      index = category.getBeginIndex();
    } else {
      PageElementLanguageLink language = pageAnalysis.getNextLanguageLink(0);
      if (language != null) {
        index = language.getBeginIndex();
      }
    }
    StringBuilder tmp = new StringBuilder();
    if (index > 0) {
      tmp.append(contents.substring(0, index));
    }
    tmp.append("{{DEFAULTSORT:");
    for (int i = 0; i < page.getTitle().length(); i++) {
      char character = page.getTitle().charAt(i);
      if (SpecialCharacters.isAuthorized(character, page.getWikipedia())) {
        tmp.append(character);
      } else {
        tmp.append(SpecialCharacters.proposeReplacement(character));
      }
    }
    tmp.append("}}\n");
    if (index < contents.length()) {
      tmp.append(contents.substring(index));
    }
    return tmp.toString();
  }
}
