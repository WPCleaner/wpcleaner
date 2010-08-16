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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TagBlock;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 78 of check wikipedia project.
 * Error 78: Reference double
 */
public class CheckErrorAlgorithm078 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm078() {
    super("Reference double");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    boolean result = false;
    int startIndex = 0;
    Map<String, TagBlock> firstTags = new HashMap<String, TagBlock>();
    Set<String> tagUsed = new TreeSet<String>();
    while (startIndex < contents.length()) {
      TagBlock tag = findNextTag(page, contents, "references", startIndex);
      if (tag != null) {
        startIndex = tag.getEndTagEndIndex();
        String group = tag.getParameter("group");
        if (group == null) {
          group = "";
        }
        TagBlock firstTag = firstTags.get(group);
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
                page,
                firstTag.getStartTagBeginIndex(), firstTag.getEndTagEndIndex(),
                ErrorLevel.CORRECT);
            errorResult.addReplacement("", GT._("Delete"));
            errors.add(errorResult);
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              page, tag.getStartTagBeginIndex(), tag.getEndTagEndIndex());
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
