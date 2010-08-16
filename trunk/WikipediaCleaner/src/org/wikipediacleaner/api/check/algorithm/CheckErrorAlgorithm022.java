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

import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 22 of check wikipedia project.
 * Error 22: Category with space
 */
public class CheckErrorAlgorithm022 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix categories"),
  };

  public CheckErrorAlgorithm022() {
    super("Category with space");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    Namespace categoryNamespace = Namespace.getNamespace(Namespace.CATEGORY, page.getWikipedia().getNamespaces());
    if (categoryNamespace == null) {
      return result;
    }
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
          boolean spaceFound = false;

          // Possible whitespaces
          int linkBegin = beginIndex + 2;
          while ((linkBegin < endIndex) && (contents.charAt(linkBegin) == ' ')) {
            spaceFound = true;
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
            int nameBegin = linkEnd + 1;
            while ((nameBegin < endIndex) && (contents.charAt(nameBegin) == ' ')) {
              spaceFound = true;
              nameBegin++;
            }
            // Category itself
            int nameEnd = nameBegin;
            while ((nameEnd < endIndex) &&
                (contents.charAt(nameEnd) != '|')) {
              nameEnd++;
            }

            // Check if category has a lower case as a first letter
            String category = contents.substring(nameBegin, nameEnd);
            if ((category.length() > 0) &&
                (spaceFound == true)) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, beginIndex, endIndex + 2);
              errorResult.addReplacement(
                  "[[" + categoryNamespace.getTitle() + ":" +
                  Character.toUpperCase(contents.charAt(nameBegin)) +
                  contents.substring(nameBegin + 1, endIndex + 2));
              errors.add(errorResult);
            }
          }
          startIndex = endIndex + 2;
        }
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
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, Page page, String contents) {
    return fixUsingFirstReplacement(fixName, page, contents);
  }
}
