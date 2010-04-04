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

package org.wikipediacleaner.api.check;

import java.util.ArrayList;

import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 17 of check wikipedia project.
 * Error 17: Category duplication
 */
public class CheckErrorAlgorithm17 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm17() {
    super("Category duplication");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    Namespace categoryNamespace = Namespace.getNamespace(Namespace.CATEGORY, page.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return result;
    }
    ArrayList<String> categories = new ArrayList<String>();
    int startIndex = 0;
    while (startIndex < contents.length()) {
      int beginIndex = contents.indexOf("[[", startIndex);
      if (beginIndex < 0) {
        startIndex = contents.length();
      } else {
        int endIndex = contents.indexOf("]]", beginIndex);
        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          // Possible whitespaces
          int linkBegin = beginIndex + 2;
          while ((linkBegin < endIndex) && (contents.charAt(linkBegin) == ' ')) {
            linkBegin++;
          }
          // Link itself
          int linkEnd = linkBegin;
          while ((linkEnd < endIndex) &&
              (contents.charAt(linkEnd) != ':') &&
              (contents.charAt(linkEnd) != '|')) {
            linkEnd++;
          }

          // Check if namespace is Category
          if ((contents.charAt(linkEnd) == ':') &&
              (categoryNamespace.isPossibleName(contents.substring(linkBegin, linkEnd)))) {
            // Possible whitespaces
            linkBegin = linkEnd + 1;
            while ((linkBegin < endIndex) && (contents.charAt(linkBegin) == ' ')) {
              linkBegin++;
            }
            // Category itself
            linkEnd = linkBegin;
            while ((linkEnd < endIndex) &&
                (contents.charAt(linkEnd) != '|')) {
              linkEnd++;
            }
            // Check if category is already known
            String category = Page.getStringUcFirst(contents.substring(linkBegin, linkEnd));
            if (categories.contains(category)) {
              if (errors == null) {
                return true;
              }
              result = true;
              errors.add(new CheckErrorResult(getShortDescription(), beginIndex, endIndex + 2));
            } else {
              categories.add(category);
            }
          }
          startIndex = endIndex + 2;
        }
      }
    }
    return result;
  }
}
