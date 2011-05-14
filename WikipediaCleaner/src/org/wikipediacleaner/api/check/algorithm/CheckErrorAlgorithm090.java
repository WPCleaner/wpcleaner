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
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 90 of check wikipedia project.
 * Error 90: DEFAULTSORT with lowercase letters
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

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      // Update position of next {{
      int beginIndex = contents.indexOf("{{", startIndex);

      if (beginIndex < 0) {
        // No more {{
        startIndex = contents.length();
      } else {
        int currentPos = beginIndex + 2;

        // Update position of next }}
        int endIndex = contents.indexOf("}}", currentPos);

        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          // Possible whitespaces
          while ((currentPos < endIndex) && Character.isWhitespace(contents.charAt(currentPos))) {
            currentPos++;
          }

          // Check that link is DEFAULTSORT
          String defaultSort = null;
          if (currentPos < endIndex) {
            MagicWord magicDefaultsort = pageAnalysis.getWikipedia().getMagicWord(
                MagicWord.DEFAULT_SORT);
            List<String> aliases = magicDefaultsort.getAliases();
            for (int i = 0; (i < aliases.size()) && (defaultSort == null); i++) {
              if (contents.startsWith(aliases.get(i), currentPos)) {
                currentPos += aliases.get(i).length();
                defaultSort = aliases.get(i);
              }
            }
          }

          // DEFAULTSORT found
          if ((currentPos < endIndex) && (defaultSort != null)) {

            // Possible whitespaces
            while ((currentPos < endIndex) && Character.isWhitespace(contents.charAt(currentPos))) {
              currentPos++;
            }

            boolean lowFound = false;
            boolean firstLetter = true;
            String text = "";
            while (currentPos < endIndex) {
              boolean error = false;
              if (Character.isLowerCase(contents.charAt(currentPos))) {
                if (firstLetter) {
                  lowFound = true;
                  error = true;
                }
                firstLetter = false;
              } else if (Character.isWhitespace(contents.charAt(currentPos))) {
                firstLetter = true;
              } else {
                firstLetter = false;
              }
              if (error) {
                text += Character.toUpperCase(contents.charAt(currentPos));
              } else {
                text += contents.charAt(currentPos);
              }
              currentPos++;
            }
            if (lowFound) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), beginIndex, endIndex + 2);
              errorResult.addReplacement("{{" + defaultSort + text + "}}");
              errors.add(errorResult);
            }
          }
          startIndex = endIndex + 2;
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
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, Page page, String contents) {
    return fixUsingFirstReplacement(fixName, page, contents);
  }
}
