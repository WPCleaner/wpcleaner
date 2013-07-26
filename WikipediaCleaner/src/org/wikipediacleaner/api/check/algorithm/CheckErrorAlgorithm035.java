/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 35 of check wikipedia project.
 * Error 35: Gallery image without description
 */
public class CheckErrorAlgorithm035 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm035() {
    super("Gallery image without description");
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

    // Retrieve image name space
    Namespace imageNamespace = pageAnalysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);

    // Analyze each gallery tag
    List<PageElementTag> galleryTags = pageAnalysis.getCompleteTags(PageElementTag.TAG_WIKI_GALLERY);
    String contents = pageAnalysis.getContents();
    boolean result = false;
    for (PageElementTag galleryTag : galleryTags) {
      if (galleryTag.getMatchingTag() != null) {
        PageElementTag endTag = galleryTag.getMatchingTag();
        int beginIndex = galleryTag.getEndIndex();
        int tmpIndex = beginIndex;
        while (tmpIndex <= endTag.getBeginIndex()) {
          if ((tmpIndex == endTag.getBeginIndex()) ||
              (contents.charAt(tmpIndex) == '\n')) {
            String line = contents.substring(beginIndex, tmpIndex).trim();
            int colonIndex = line.indexOf(':');
            if ((colonIndex > 0) && (imageNamespace.isPossibleName(line.substring(0, colonIndex)))) {
              int pipeIndex = line.indexOf('|', colonIndex);
              boolean description = false;
              if ((pipeIndex >= 0) && (pipeIndex + 1 < line.length())) {
                if (line.substring(pipeIndex + 1).trim().length() > 0) {
                  description = true;
                }
              }
              if (!description) {
                if (errors == null) {
                  return true;
                }
                result = true;

                CheckErrorResult errorResult = createCheckErrorResult(
                    pageAnalysis.getPage(), beginIndex, tmpIndex);
                errors.add(errorResult);
              }
            }
            beginIndex = tmpIndex + 1;
          }
          tmpIndex++;
        }
      }
    }
    return result;
  }
}
