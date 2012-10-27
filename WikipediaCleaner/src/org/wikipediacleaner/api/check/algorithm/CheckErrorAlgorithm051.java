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
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 51 of check wikipedia project.
 * Error 51: Interwiki before last headline.
 */
public class CheckErrorAlgorithm051 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm051() {
    super("Interwiki before last headline");
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
    if ((pageAnalysis == null) || (pageAnalysis.getPage() == null)) {
      return false;
    }
    if (!pageAnalysis.getPage().isArticle()) {
      return false;
    }

    // Retrieving last headline
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    if (titles.size() == 0) {
      return false;
    }
    int lastTitle = titles.get(titles.size() - 1).getEndIndex();

    // Checking every language link
    List<PageElementLanguageLink> languages = pageAnalysis.getLanguageLinks();
    boolean result = false;
    for (PageElementLanguageLink language : languages) {
      if (language.getBeginIndex() >= lastTitle) {
        return result;
      }
      if (errors == null) {
        return true;
      }
      result = true;
      CheckErrorResult errorResult = createCheckErrorResult(
          pageAnalysis.getPage(),
          language.getBeginIndex(), language.getEndIndex());
      errors.add(errorResult);
    }

    return result;
  }
}
