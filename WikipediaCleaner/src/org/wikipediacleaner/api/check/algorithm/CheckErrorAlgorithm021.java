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
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckCategoryLinkActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 21 of check wikipedia project.
 * Error 21: Category is english
 */
public class CheckErrorAlgorithm021 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm021() {
    super("Category is english");
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

    // Retrieve preferred category name
    String preferredCategory = pageAnalysis.getWikipedia().getCheckWikiProperty(
        "category", 21, true, false, false);

    // Analyze the text from the begining
    int startIndex = 0;
    boolean result = false;
    Namespace categoryNamespace = Namespace.getNamespace(
        Namespace.CATEGORY, pageAnalysis.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return false;
    }
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      PageElementCategory category = PageContents.findNextCategory(
          pageAnalysis.getPage(), contents, startIndex, pageAnalysis.getComments());
      if (category == null) {
        startIndex = contents.length();
      } else {
        if ("Category".equalsIgnoreCase(category.getCategory())) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), category.getBeginIndex(), category.getEndIndex());
          errorResult.addPossibleAction(
              GT._("Check category"),
              new CheckCategoryLinkActionProvider(
                  EnumWikipedia.EN, pageAnalysis.getWikipedia(),
                  category.getName(), category.getSort()));
          List<String> replacements = new ArrayList<String>();
          if ((preferredCategory != null) && (categoryNamespace.isPossibleName(preferredCategory))) {
            replacements.add(preferredCategory);
          }
          if (!replacements.contains(categoryNamespace.getCanonicalTitle())) {
            replacements.add(categoryNamespace.getCanonicalTitle());
          }
          for (String alias : categoryNamespace.getAliases()) {
            if (!replacements.contains(alias)) {
              replacements.add(alias);
            }
          }
          for (String replacement : replacements) {
            if (!"Category".equalsIgnoreCase(replacement)) {
              errorResult.addReplacement(
                  "[[" + replacement + ":" + category.getName() +
                  ((category.getSort() != null) ? "|" + category.getSort() : "") + "]]");
            }
          }
          errorResult.addReplacement(
              "<!-- " + contents.substring(category.getBeginIndex(), category.getEndIndex()) + " -->",
              GT._("Comment category out"));
          errors.add(errorResult);
        }
        startIndex = category.getEndIndex();
      }
    }
    return result;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("category", GT._(
        "The preferred name to use for replacing {0}", "[[Category:<i>xxxx</i>]]"));
    return parameters;
  }
}
