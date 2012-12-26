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
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 90 of check wikipedia project.
 * Error 90: DEFAULTSORT with lower case letters
 */
public class CheckErrorAlgorithm090 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix DEFAULTSORT"),
  };

  public CheckErrorAlgorithm090() {
    super("DEFAULTSORT with lowercase letters");
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

    // Check every DEFAULTSORT
    List<PageElementFunction> defaultSorts = pageAnalysis.getDefaultSorts();
    boolean result = false;
    for (PageElementFunction defaultSort : defaultSorts) {

      // Check if a lower case character is at the beginning of a word
      boolean firstLetter = true;
      StringBuilder newText = null;
      String text = (defaultSort.getParameterCount() > 0) ? defaultSort.getParameterValue(0) : "";
      for (int index = 0; index < text.length(); index++) {
        char currentChar = text.charAt(index);
        if (Character.isUpperCase(currentChar)) {
          firstLetter = false;
        } else if (Character.isLowerCase(currentChar)) {
          if (firstLetter) {
            if (newText == null) {
              newText = new StringBuilder(text.substring(0, index));
            }
            currentChar = Character.toUpperCase(currentChar);
          }
          firstLetter = false;
        } else if (Character.isWhitespace(currentChar)) {
          firstLetter = true;
        } else {
          firstLetter = false;
        }
        if (newText != null) {
          newText.append(currentChar);
        }
      }

      // Register error
      if (newText != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            defaultSort.getBeginIndex(), defaultSort.getEndIndex());
        errorResult.addReplacement(PageElementFunction.createFunction(
            defaultSort.getFunctionName(), newText.toString()));
        errors.add(errorResult);
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
