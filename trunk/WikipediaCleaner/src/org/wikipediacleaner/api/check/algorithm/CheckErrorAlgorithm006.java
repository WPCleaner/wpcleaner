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
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 06 of check wikipedia project.
 * Error 06: DEFAULTSORT with special letters
 */
public class CheckErrorAlgorithm006 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix DEFAULTSORT"),
  };

  public CheckErrorAlgorithm006() {
    super("DEFAULTSORT with special letters");
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

    // Analyzing the text from the beginning
    Collection<PageElementDefaultsort> tags = pageAnalysis.getDefaultSorts();
    if ((tags == null) || (tags.isEmpty())) {
      return false;
    }
    boolean result = false;
    EnumWikipedia wiki = pageAnalysis.getWikipedia();
    for (PageElementDefaultsort tag : tags) {
      if (tag != null) {
        boolean characterFound = false;
        boolean characterReplaced = false;
        String unknownCharacters = "";
        String text = "";
        int currentPos = 0;
        String value = tag.getValue();
        while (currentPos < value.length()) {
          boolean error = false;
          char character = value.charAt(currentPos);
          if (!SpecialCharacters.isAuthorized(character, wiki)) {
            characterFound = true;
            error = true;
          }
          if (error) {
            String newCharacter = SpecialCharacters.proposeReplacement(character, wiki);
            if (!Character.toString(character).equals(newCharacter)) {
              characterReplaced = true;
            } else {
              unknownCharacters += character;
            }
            text += newCharacter;
          } else {
            text += character;
          }
          currentPos++;
        }
        if (characterFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), tag.getBeginIndex(), tag.getEndIndex());
          if (characterReplaced) {
            errorResult.addReplacement("{{" + tag.getTag() + text + "}}");
          } else {
            errorResult.addPossibleAction(
                GT._("Unable to replace the characters [{0}]", unknownCharacters),
                new NullActionProvider());
          }
          errors.add(errorResult);
        }
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
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
