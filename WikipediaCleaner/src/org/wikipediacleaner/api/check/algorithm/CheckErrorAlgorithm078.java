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
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 78 of check wikipedia project.
 * Error 78: Reference double
 */
public class CheckErrorAlgorithm078 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm078() {
    super("Reference double");
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
    int startIndex = 0;
    Map<String, PageElementTag> firstTags = new HashMap<String, PageElementTag>();
    Set<String> tagUsed = new TreeSet<String>();
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      PageElementTag tag = PageContents.findNextTag(
          pageAnalysis.getPage(), contents, "references", startIndex);
      if (tag != null) {
        startIndex = tag.getEndTagEndIndex();
        String group = tag.getParameter("group");
        if (group == null) {
          group = "";
        }
        PageElementTag firstTag = firstTags.get(group);
        if (firstTag == null) {
          firstTags.put(group, tag);
        } else {
          if (errors == null) {
            return true;
          }
          result = true;
          if (!tagUsed.contains(group)) {
            tagUsed.add(group);
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(),
                firstTag.getStartTagBeginIndex(), firstTag.getEndTagEndIndex(),
                ErrorLevel.CORRECT);
            errorResult.addReplacement("", GT._("Delete"));
            errors.add(errorResult);
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              tag.getStartTagBeginIndex(), tag.getEndTagEndIndex());
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
