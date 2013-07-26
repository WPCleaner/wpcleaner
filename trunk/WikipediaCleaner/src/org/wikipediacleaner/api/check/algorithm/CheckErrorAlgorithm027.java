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

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 27 of check wikipedia project.
 * Error 27: Unicode syntax
 */
public class CheckErrorAlgorithm027 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Replace all"),
  };

  public CheckErrorAlgorithm027() {
    super("Unicode syntax");
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
    boolean result = false;
    String contents = pageAnalysis.getContents();
    int ampersandIndex = contents.indexOf('&');
    int maxLength = contents.length();
    while ((ampersandIndex >= 0) && (ampersandIndex < maxLength)) {
      // TODO : Check if we should look for a match a this position
      int tmpIndex = ampersandIndex + 1;
      if ((tmpIndex < maxLength) && (contents.charAt(tmpIndex) == '#')) {
        tmpIndex++;
      }
      int radix = 10;
      if ((tmpIndex < maxLength) && (contents.charAt(tmpIndex) == 'x')) {
        radix = 16;
        tmpIndex++;
      }
      int startIndex = tmpIndex;
      while ((tmpIndex < maxLength) &&
             (Character.digit(contents.charAt(tmpIndex), radix) >= 0)) {
        tmpIndex++;
      }
      if ((tmpIndex > startIndex) &&
          (tmpIndex < maxLength) &&
          (contents.charAt(tmpIndex) == ';')) {
        int entityNumber = Integer.parseInt(contents.substring(startIndex, tmpIndex), radix);
        HtmlCharacters htmlCharacter = HtmlCharacters.getCharacterByEntityNumber(entityNumber);
        boolean shouldReplace = true;
        if (htmlCharacter != null) {
          shouldReplace = htmlCharacter.shouldReplaceNumeric();
          if (HtmlCharacters.SYMBOL_VERTICAL_BAR.equals(htmlCharacter) &&
              (pageAnalysis.isInTemplate(ampersandIndex) != null)) {
            shouldReplace = false;
          }
        }
        if (shouldReplace) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), ampersandIndex, tmpIndex + 1,
              htmlCharacter != null ? ErrorLevel.ERROR : ErrorLevel.WARNING);
          if (htmlCharacter != null) {
            errorResult.addReplacement("" + htmlCharacter.getValue());
          }
          errors.add(errorResult);
        }
      }
      ampersandIndex = contents.indexOf('&', ampersandIndex + 1);
    }

    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public String botFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
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
