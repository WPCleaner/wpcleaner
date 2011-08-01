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
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 30 of check wikipedia project.
 * Error 30: Image without description
 */
public class CheckErrorAlgorithm030 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm030() {
    super("Image without description");
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
    for (PageElementImage image : pageAnalysis.getImages()) {
      String description = image.getDescription();
      if ((description == null) || (description.trim().length() == 0)) {
        String alt = image.getAlternateDescription();
        if ((alt == null) || (alt.trim().length() == 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), image.getBeginIndex(), image.getEndIndex());
          errorResult.addPossibleAction(new SimpleAction(
              GT._("View image"),
              new PageViewAction(
                  image.getNamespace() + ":" + image.getImage(),
                  pageAnalysis.getWikipedia(),
                  true)));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
