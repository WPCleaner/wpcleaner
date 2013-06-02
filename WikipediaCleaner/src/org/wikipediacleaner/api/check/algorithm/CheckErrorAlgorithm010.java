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
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 10 of check wikipedia project.
 * Error 10: Square brackets not correct end
 */
public class CheckErrorAlgorithm010 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm010() {
    super("Square brackets not correct end");
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

    // Analyze contents from the beginning
    String contents = pageAnalysis.getContents();
    int maxLength = contents.length();
    int currentIndex = contents.indexOf("[[");
    boolean result = false;
    while (currentIndex >= 0) {
      boolean shouldCount = true;
      if (shouldCount) {
        PageElementInternalLink link = pageAnalysis.isInInternalLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementImage image = pageAnalysis.isInImage(currentIndex);
        if ((image != null) && (image.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementCategory category = pageAnalysis.isInCategory(currentIndex);
        if ((category != null) && (category.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementLanguageLink link = pageAnalysis.isInLanguageLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementInterwikiLink link = pageAnalysis.isInInterwikiLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementExternalLink link = pageAnalysis.isInExternalLink(currentIndex + 1);
        if ((link != null) && (link.getBeginIndex() == currentIndex + 1)) {
          shouldCount = false;
        }
      }
      if (shouldCount &&
          (pageAnalysis.isInComment(currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
          (pageAnalysis.isInTag(currentIndex) != null)) {
        shouldCount = false;
      }
      if (shouldCount) {
        if (errors == null) {
          return true;
        }
        result = true;
        
        // Check if there is a potential end
        int tmpIndex = currentIndex + 2;
        boolean errorReported = false;
        boolean finished = false;
        while (!finished && (tmpIndex < maxLength)) {
          char tmpChar = contents.charAt(tmpIndex);
          if ((tmpChar == '\n') ||
              (tmpChar == '[') ||
              (tmpChar == '{')) {
            finished = true;
          } else if (tmpChar == ']') {
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), currentIndex, tmpIndex + 1);

            // Check if the situation is something like [[http://....] (replacement: [http://....])
            List<String> protocols = PageElementExternalLink.getProtocols();
            boolean protocolFound = false;
            for (String protocol : protocols) {
              if (contents.startsWith(protocol, currentIndex + 2)) {
                protocolFound = true;
              }
            }
            if (protocolFound) {
              errorResult.addReplacement(contents.substring(currentIndex + 1, tmpIndex + 1));
            }

            errorResult.addReplacement(contents.substring(currentIndex, tmpIndex + 1) + "]");
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          } else if (tmpChar == '}') {
            int lastChar = tmpIndex;
            if ((lastChar + 1 < maxLength) && (contents.charAt(lastChar + 1) == '}')) {
              lastChar++;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), currentIndex, lastChar + 1);
            errorResult.addReplacement(contents.substring(currentIndex, tmpIndex) + "]]");
            errorResult.addReplacement("{{" + contents.substring(currentIndex + 2, tmpIndex) + "}}");
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          }
          tmpIndex++;
        }

        // Default
        if (!errorReported) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), currentIndex, currentIndex + 2);
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      }
      currentIndex = contents.indexOf("[[", currentIndex + 2);
    }

    return result;
  }
}
