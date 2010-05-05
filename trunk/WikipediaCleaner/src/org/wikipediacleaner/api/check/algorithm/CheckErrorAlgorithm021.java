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

import org.wikipediacleaner.api.check.CheckCategoryLinkActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 21 of check wikipedia project.
 * Error 21: Category is english
 */
public class CheckErrorAlgorithm021 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm021() {
    super("Category is english");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    int startIndex = 0;
    boolean result = false;
    Namespace categoryNamespace = Namespace.getNamespace(Namespace.CATEGORY, page.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return result;
    }
    while (startIndex < contents.length()) {
      if (contents.startsWith("[[", startIndex)) {
        int beginIndex = startIndex;
        int currentIndex = beginIndex + 2;

        // Namespace
        int linkIndex = currentIndex;
        while ((currentIndex < contents.length()) &&
               (contents.charAt(currentIndex) != ':') &&
               (contents.charAt(currentIndex) != '|') &&
               (contents.charAt(currentIndex) != ']') &&
               (contents.charAt(currentIndex) != '[')) {
          currentIndex++;
        }

        // Check if namespace is Category
        if ((currentIndex < contents.length()) &&
            (contents.charAt(currentIndex) == ':') &&
            (categoryNamespace.isPossibleName(contents.substring(linkIndex, currentIndex).trim()))) {
          String category = contents.substring(linkIndex, currentIndex);

          // Link itself
          currentIndex++;
          linkIndex = currentIndex;
          while ((currentIndex < contents.length()) &&
                 (contents.charAt(currentIndex) != '|') &&
                 (contents.charAt(currentIndex) != ']')) {
            currentIndex++;
          }

          // Retrieve category name
          String categoryName = Page.getStringUcFirst(contents.substring(linkIndex, currentIndex).trim());

          // Sort order
          String order = null;
          if ((currentIndex < contents.length()) &&
              (contents.charAt(currentIndex) == '|')) {
            currentIndex++;
            linkIndex = currentIndex;
            while ((currentIndex < contents.length()) &&
                   (contents.charAt(currentIndex) != ']')) {
              currentIndex++;
            }
            order = contents.substring(linkIndex, currentIndex);
          }

          // Go to the end
          while ((currentIndex < contents.length()) &&
                 (!contents.startsWith("]]", currentIndex))) {
            currentIndex++;
          }
          if ((currentIndex < contents.length()) &&
              (contents.startsWith("]]", currentIndex))) {
            currentIndex += 2;
            if ("Category".equals(category)) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = new CheckErrorResult(
                  getShortDescription(), beginIndex, currentIndex);
              errorResult.addPossibleAction(
                  GT._("Check category"),
                  new CheckCategoryLinkActionProvider(
                      EnumWikipedia.EN, page.getWikipedia(),
                      categoryName, order));
              if (!"Category".equals(categoryNamespace.getCanonicalTitle())) {
                errorResult.addReplacement(
                    "[[" + categoryNamespace.getCanonicalTitle() + ":" + categoryName +
                    ((order != null) ? "|" + order : "") + "]]");
              }
              for (String alias : categoryNamespace.getAliases()) {
                if (!"Category".equals(alias)) {
                  errorResult.addReplacement(
                      "[[" + alias + ":" + categoryName +
                      ((order != null) ? "|" + order : "") + "]]");
                }
              }
              errors.add(errorResult);
            }
          }
        }
        startIndex = currentIndex;
      } else {
        startIndex++;
      }
    }
    return result;
  }
}
