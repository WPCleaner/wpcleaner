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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 2 of check wikipedia project.
 * Error 2: Article with false <br/>
 */
public class CheckErrorAlgorithm002 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix all <br/> tags"),
  };

  public CheckErrorAlgorithm002() {
    super("Article with false <br/>");
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

    boolean result = false;
    int currentIndex = 0;
    String contents = pageAnalysis.getContents();
    String br = PageElementTag.TAG_HTML_BR;
    int maxSize = contents.length();
    while (currentIndex < maxSize) {
      int nextIndex = currentIndex + 1;
      boolean shouldCheck = true;

      // Check if we are in a comment
      if (shouldCheck) {
        PageElementComment comment = pageAnalysis.isInComment(currentIndex);
        if (comment != null) {
          shouldCheck = false;
          nextIndex = comment.getEndIndex();
        }
      }

      // Check if this is a br tag
      if ((shouldCheck) && (contents.charAt(currentIndex) == '<')) {
        int tmpIndex = currentIndex + 1;
        while ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        boolean incorrectChar = false;
        while ((tmpIndex < maxSize) && (" \\.:?/".indexOf(contents.charAt(tmpIndex)) >= 0)) {
          tmpIndex++;
          incorrectChar = true;
        }
        boolean brTag = true;
        for (int i = 0; i < br.length(); i++) {
          if ((tmpIndex >= maxSize) ||
              (Character.toUpperCase(contents.charAt(tmpIndex)) != Character.toUpperCase(br.charAt(i)))) {
            brTag = false;
          }
          tmpIndex++;
        }
        if ((tmpIndex < maxSize) && (brTag)) {
          while ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == ' ')) {
            tmpIndex++;
          }
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '/')) {
            tmpIndex++;
          }
          while ((tmpIndex < maxSize) && (" \\.:?/".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
            incorrectChar = true;
          }
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '>')) {
            if (incorrectChar) {
              if (errors == null) {
                return true;
              }
              result = true;
              tmpIndex++;
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), currentIndex, tmpIndex);
              errorResult.addReplacement("<br />");
              errors.add(errorResult);
              nextIndex = tmpIndex;
            }
          }
        }
      }

      currentIndex = nextIndex;
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
