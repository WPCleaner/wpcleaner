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
import java.util.HashMap;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 17 of check wikipedia project.
 * Error 17: Category duplication
 */
public class CheckErrorAlgorithm017 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm017() {
    super("Category duplication");
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
    HashMap<String, CategoryElement> categories = new HashMap<String, CategoryElement>();
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

          // Go to the end
          while ((currentIndex < contents.length()) &&
                 (!contents.startsWith("]]", currentIndex))) {
            currentIndex++;
          }
          if ((currentIndex < contents.length()) &&
              (contents.startsWith("]]", currentIndex))) {
            currentIndex += 2;
            CategoryElement categoryElement = categories.get(categoryName);
            if (categoryElement == null) {
              categoryElement = new CategoryElement(categoryName, beginIndex, currentIndex);
              categories.put(categoryName, categoryElement);
            } else {
              if (errors == null) {
                return true;
              }
              result = true;
              if (categoryElement.errorResult == null) {
                categoryElement.errorResult = new CheckErrorResult(
                    getShortDescription(),
                    categoryElement.begin, categoryElement.end,
                    CheckErrorResult.ErrorLevel.CORRECT);
                errors.add(categoryElement.errorResult);
              }
              CheckErrorResult errorResult = new CheckErrorResult(
                  getShortDescription(), beginIndex, currentIndex);
              errorResult.addReplacement("");
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

  /**
   * Class to hold information about a category element.
   */
  static class CategoryElement {
    String name;
    int begin;
    int end;
    CheckErrorResult errorResult;

    /**
     * @param name Category name.
     * @param begin Begin position.
     * @param end End position.
     */
    public CategoryElement(String name, int begin, int end) {
      this.name = name;
      this.begin = begin;
      this.end = end;
    }
  }
}
