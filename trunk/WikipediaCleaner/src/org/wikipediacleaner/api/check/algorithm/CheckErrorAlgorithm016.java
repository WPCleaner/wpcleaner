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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 16 of check wikipedia project.
 * Error 16: Template with Unicode control characters
 */
public class CheckErrorAlgorithm016 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Remove all control characters"),
  };

  public CheckErrorAlgorithm016() {
    super("Template with Unicode control characters");
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

    Collection<PageElementTemplate> templates = pageAnalysis.getTemplates();
    if (templates == null) {
      return false;
    }
    boolean result = false;
    String contents = pageAnalysis.getContents();
    for (PageElementTemplate template : templates) {
      int begin = template.getBeginIndex();
      int end = template.getEndIndex();
      boolean found = false;
      for (int index = begin; index < end; index++) {
        char character = contents.charAt(index);
        if ((character == (char) 0xFEFF) ||
            (character == (char) 0x200E) ||
            (character == (char) 0x200B)) {
          found = true;
        }
      }
      if (found) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(pageAnalysis.getPage(), begin, end);
        StringBuilder replacement = new StringBuilder();
        for (int index = begin; index < end; index++) {
          char character = contents.charAt(index);
          if ((character != (char) 0xFEFF) &&
              (character != (char) 0x200E) &&
              (character != (char) 0x200B)) {
            replacement.append(character);
          }
        }
        errorResult.addReplacement(replacement.toString(), GT._("Remove all control characters"));
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
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, Page page, String contents, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, page, contents);
  }
}
