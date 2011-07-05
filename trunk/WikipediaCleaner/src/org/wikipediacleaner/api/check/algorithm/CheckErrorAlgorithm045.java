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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementLanguageLink;


/**
 * Algorithm for analyzing error 45 of check wikipedia project.
 * Error 45: Interwiki double
 */
public class CheckErrorAlgorithm045 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm045() {
    super("Interwiki double");
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
    HashMap<String, PageElementLanguageLink> links = new HashMap<String, PageElementLanguageLink>();
    ArrayList<String> linksTwice = new ArrayList<String>();
    for (PageElementLanguageLink link : pageAnalysis.getLanguageLinks()) {
      String index = link.getLanguage() + ":" + link.getLink();
      PageElementLanguageLink existingLink = links.get(index);
      if (existingLink == null) {
        links.put(index, link);
      } else {
        if (errors == null) {
          return true;
        }
        result = true;
        if (!linksTwice.contains(index)) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              existingLink.getBeginIndex(),
              existingLink.getEndIndex(),
              CheckErrorResult.ErrorLevel.CORRECT);
          errors.add(errorResult);
          linksTwice.add(index);
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            link.getBeginIndex(),
            link.getEndIndex());
        errorResult.addReplacement("");
        errors.add(errorResult);
      }
    }

    return result;
  }
}
