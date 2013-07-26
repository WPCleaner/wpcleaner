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
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 44 of check wikipedia project.
 * Error 44: Headlines with bold
 */
public class CheckErrorAlgorithm044 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix all bold headlines"),
  };

  public CheckErrorAlgorithm044() {
    super("Headlines with bold");
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

    // Check every title
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    boolean result = false;
    for (PageElementTitle title : titles) {
      String text = title.getTitle();
      if (text != null) {
        text = text.trim();

        // Check if the title is bold
        int index = 0;
        int countBold = 0;
        while (index < text.length()) {
          if (text.startsWith("'''", index)) {
            index += 3;
            countBold++;
          } else {
            index++;
          }
        }

        // Register error
        if (countBold > 1) {
          if (errors == null) {
            return true;
          }
          result = true;
          text = text.replaceAll("'''", "");
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              title.getBeginIndex(), title.getEndIndex());
          errorResult.addReplacement(PageElementTitle.createTitle(title.getLevel(), text));
          errors.add(errorResult);
        }
      }
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
