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

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 65 of check wikipedia project.
 * Error 65: Image description with break
 */
public class CheckErrorAlgorithm065 extends CheckErrorAlgorithmBase {

  private final static String[] possibleBreaks = { "<br>", "<br/>", "</br>", "<br />" };

  public CheckErrorAlgorithm065() {
    super("Image description with break");
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
    Namespace imageNamespace = Namespace.getNamespace(Namespace.IMAGE, page.getWikipedia().getNamespaces());
    if (imageNamespace == null) {
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

        // Check if namespace is Image
        if ((currentIndex < contents.length()) &&
            (contents.charAt(currentIndex) == ':') &&
            (imageNamespace.isPossibleName(contents.substring(linkIndex, currentIndex).trim()))) {

          // Link itself
          currentIndex++;
          while ((currentIndex < contents.length()) &&
                 (contents.charAt(currentIndex) != '|') &&
                 (contents.charAt(currentIndex) != ']')) {
            currentIndex++;
          }

          // Go to the end
          if ((currentIndex < contents.length()) &&
              (contents.charAt(currentIndex) == '|')) {
            currentIndex++;
          }
          linkIndex = currentIndex;
          while ((currentIndex < contents.length()) &&
                 (!contents.startsWith("]]", currentIndex))) {
            currentIndex++;
          }
          if ((currentIndex < contents.length()) &&
              (contents.startsWith("]]", currentIndex))) {
            EnumWikipedia wikipedia = page.getWikipedia();
            for (int tmpIndex = linkIndex; tmpIndex <= currentIndex; tmpIndex++) {
              if ((contents.charAt(tmpIndex) == '|') ||
                  (contents.charAt(tmpIndex) == ']')) {
                String arg = contents.substring(linkIndex, tmpIndex);
                if (!wikipedia.isPossibleAliasForImgMagicWord(arg)) {
                  String breakFound = null;
                  for (String possibleBreak : possibleBreaks) {
                    if (arg.endsWith(possibleBreak)) {
                      breakFound = possibleBreak;
                    }
                  }
                  if (breakFound != null) {
                    if (errors == null) {
                      return true;
                    }
                    result = true;
                    CheckErrorResult errorResult = createCheckErrorResult(
                        page, tmpIndex - breakFound.length(), tmpIndex);
                    errorResult.addReplacement("");
                    errors.add(errorResult);
                  }
                }
                tmpIndex++;
                linkIndex = tmpIndex;
              }
            }
            currentIndex += 2;
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
