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
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 8 of check wikipedia project.
 * Error 8: Headline should end with "="
 */
public class CheckErrorAlgorithm008 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm008() {
    super("Headline should end with \"=\"");
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
    // TODO: Change completely as titles not ending with "=" are not considered as titles
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    String contents = pageAnalysis.getContents();
    boolean result = false;
    for (PageElementTitle title : titles) {

      // Check if text is present after title
      int equalIndex = title.getEndIndex() - 1;
      boolean textFound = false;
      while ((equalIndex > title.getBeginIndex()) &&
             (contents.charAt(equalIndex) != '=')) {
        if (!Character.isWhitespace(contents.charAt(equalIndex))) {
          PageElementComment comment = pageAnalysis.isInComment(equalIndex);
          if (comment != null) {
            equalIndex = comment.getBeginIndex();
          } else {
            textFound = true;
          }
        }
        equalIndex--;
      }

      // Create error
      if (textFound) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Report detailed result
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            title.getBeginIndex(), title.getEndIndex());

        // Replacement : truncate if there's text after end title
        if (equalIndex > title.getBeginIndex() + title.getLevel()) {
          errorResult.addReplacement(contents.substring(title.getBeginIndex(), equalIndex + 1));
        }

        // Replacement : complete line
        String replacement = contents.substring(title.getBeginIndex(), title.getEndIndex());
        for (int i = 0; i < title.getLevel(); i++) {
          replacement += "=";
        }
        errorResult.addReplacement(replacement);

        errors.add(errorResult);
      }
    }

    return result;
  }
}
