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
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
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
          String imageName = contents.substring(linkIndex, currentIndex);

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
            String[] args = contents.substring(linkIndex, currentIndex).split("\\|");
            currentIndex += 2;
            EnumWikipedia wikipedia = page.getWikipedia();
            boolean descriptionFound = false;
            for (int i = args.length; (i > 0) && !descriptionFound; i--) {
              String arg = args[i - 1];
              if (arg.length() > 0) {
                boolean magicWordFound = wikipedia.isPossibleAliasForImgMagicWord(arg);
                if (!magicWordFound) {
                  descriptionFound = true;
                }
              }
            }
            if (!descriptionFound) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, beginIndex, currentIndex);
              errorResult.addPossibleAction(new SimpleAction(
                  GT._("View image"),
                  new PageViewAction(imageName, wikipedia, true)));
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
