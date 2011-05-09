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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.Suggestion;


/**
 * Algorithm for analyzing error 500 of check wikipedia project.
 * Error 92: Orthograph and typography
 */
public class CheckErrorAlgorithm501 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm501() {
    super("Orthograph and typography");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments in the page contents.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    Map<String, Suggestion> suggestions = page.getWikipedia().getSuggestions();
    if ((suggestions == null) || (suggestions.isEmpty())) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    List<Suggestion> possibles = new ArrayList<Suggestion>();
    while (startIndex < contents.length()) {
      possibles.clear();

      // Test every suggestion
      int maxLength = 0;
      for (Suggestion suggestion : suggestions.values()) {
        Matcher matcher = suggestion.lookingAt(contents, startIndex);
        if (matcher != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          possibles.add(suggestion);
          if (matcher.end() > maxLength) {
            maxLength = matcher.end();
          }
        }
      }

      // Analyze matching suggestions
      if (!possibles.isEmpty()) {
        CheckErrorResult error = createCheckErrorResult(
            page, startIndex, startIndex + maxLength);
        String text = contents.substring(startIndex, startIndex + maxLength);
        List<String> replacements = new ArrayList<String>();
        List<String> commentList = new ArrayList<String>();
        for (Suggestion suggestion : possibles) {
          replacements.addAll(suggestion.getReplacements(text));
          if (suggestion.getComment() != null) {
            commentList.add(suggestion.getComment());
          }
        }
        Collections.sort(replacements);
        for (String replacement : replacements) {
          error.addReplacement(replacement);
        }
        for (String comment : commentList) {
          error.addPossibleAction(comment, new NullActionProvider());
        }
        errors.add(error);
      }

      // Go to the next non letter/digit character
      while ((startIndex < contents.length()) &&
             (Character.isLetterOrDigit(contents.charAt(startIndex)))) {
        startIndex++;
      }
      startIndex++;
    }
    return result;
  }
}
