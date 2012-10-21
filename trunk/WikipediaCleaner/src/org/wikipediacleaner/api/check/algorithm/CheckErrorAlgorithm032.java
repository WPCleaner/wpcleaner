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

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;


/**
 * Algorithm for analyzing error 32 of check wikipedia project.
 * Error 32: Double pipe in one link.
 */
public class CheckErrorAlgorithm032 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm032() {
    super("Double pipe in one link");
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
    Namespace fileNamespace = pageAnalysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);
    for (PageElementInternalLink link : pageAnalysis.getInternalLinks()) {
      // Finding possible namespace
      String namespace = null;
      if (link.getLink() != null) {
        int colonPos = link.getLink().indexOf(':');
        if (colonPos > 0) {
          namespace = link.getLink().substring(0, colonPos).trim();
        }
      }

      // Analyze link text
      String text = link.getText();
      if ((text != null) &&
          ((namespace == null) || (!fileNamespace.isPossibleName(namespace)))) {
        int levelSquareBrackets = 0;
        int levelCurlyBrackets = 0;
        ArrayList<Integer> pipeIndex = new ArrayList<Integer>();
        int currentPos = 0;
        while (currentPos < text.length()) {
          switch (text.charAt(currentPos)) {
          case '[':
            // Checking if we have a [[
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == '[')) {
              levelSquareBrackets++;
              currentPos++;
            }
            break;
          case ']':
            // Checking if we have a ]]
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == ']')) {
              levelSquareBrackets--;
              currentPos++;
            }
            break;
          case '{':
            // Checking if we have a {{
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == '{')) {
              levelCurlyBrackets++;
              currentPos++;
            }
            break;
          case '}':
            // Checking if we have a }}
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == '}')) {
              levelCurlyBrackets--;
              currentPos++;
            }
            break;
          case '|':
            // Checking if the | is counting for
            if ((levelSquareBrackets == 0) &&
                (levelCurlyBrackets == 0)) {
              pipeIndex.add(Integer.valueOf(currentPos));
            }
            break;
          }
          currentPos++;
        }

        // Testing if the error has been found
        if ((levelSquareBrackets == 0) && (pipeIndex.size() > 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
          for (int i = 0; i <= pipeIndex.size(); i++) {
            int beginText = (i > 0) ? (pipeIndex.get(i - 1).intValue() + 1) : 0;
            int endText = (i < pipeIndex.size()) ? pipeIndex.get(i).intValue() : text.length();
            if ((beginText + 1 < endText) &&
                (text.substring(beginText + 1, endText).trim().length() > 0)) {
              errorResult.addReplacement(
                  "[[" + link.getLink() + "|" + text.substring(beginText, endText) + "]]");
            }
          }
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
