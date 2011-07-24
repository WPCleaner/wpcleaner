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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.Suggestion;


/**
 * Algorithm for analyzing error 501 of check wikipedia project.
 * Error 501: Orthograph and typography
 */
public class CheckErrorAlgorithm501 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm501() {
    super("Orthograph and typography");
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
    if (!pageAnalysis.shouldCheckOrthograph()) {
      return false;
    }
    Map<String, Suggestion> suggestions = pageAnalysis.getWikipedia().getSuggestions();
    if ((suggestions == null) || (suggestions.isEmpty())) {
      return false;
    }

    // Initialize matchers
    Map<Suggestion, Matcher> matchers = new HashMap<Suggestion, Matcher>(suggestions.size());
    for (Suggestion suggestion : suggestions.values()) {
      Matcher matcher = suggestion.initMatcher(pageAnalysis.getContents());
      matchers.put(suggestion, matcher);
    }

    // Initialize iterators to the various page elements
    List<Suggestion> possibles = new ArrayList<Suggestion>();
    String contents = pageAnalysis.getContents();
    Collection<PageElement> elements = pageAnalysis.getElements(
        true, true, true, true, true, true, true, true, true, false);
    Iterator<PageElement> itElement = elements.iterator();
    PageElement currentElement = itElement.hasNext() ? itElement.next() : null;

    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {

      // Test if the position is correct to check orthograph
      int nextIndex = startIndex + 1;
      boolean checkOrthograph = true;

      // If the position is inside an element, go after
      // TODO : be more specific ? for example, checking in an image description
      if (checkOrthograph && (currentElement != null)) {
        while ((currentElement != null) &&
               (startIndex >= currentElement.getEndIndex())) {
          currentElement = itElement.hasNext() ? itElement.next() : null;
        }
        if ((currentElement != null) &&
            (startIndex >= currentElement.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentElement.getEndIndex());
        }
      }

      // If the position is inside a tag (<tag>, </tag> or <tag/>), go after
      // TODO

      // If the position is inside a table definition, go after
      // TODO

      // Check orthograph
      if (checkOrthograph) {
        possibles.clear();
  
        // Test every suggestion
        int maxLength = 0;
        int contentsLength = contents.length();
        for (Entry<Suggestion, Matcher> entry : matchers.entrySet()) {
          Matcher matcher = entry.getValue();
          if (matcher.region(startIndex, contentsLength).lookingAt()) {
            int pos = matcher.end();
            if ((pos >= contents.length()) ||
                (!Character.isLetterOrDigit(contents.charAt(pos)))) {
              if (errors == null) {
                return true;
              }
              result = true;
              if (pos - startIndex >= maxLength) {
                if (pos - startIndex > maxLength) {
                  possibles.clear();
                  maxLength = pos - startIndex;
                }
                possibles.add(entry.getKey());
              }
            }
          }
        }
  
        // Analyze matching suggestions
        if (!possibles.isEmpty()) {
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(), startIndex, startIndex + maxLength);
          String text = contents.substring(
              startIndex, startIndex + maxLength);
          for (Suggestion suggestion : possibles) {
            String comment = suggestion.getComment();
            if (comment != null) {
              error.addPossibleAction(comment, new NullActionProvider());
            }
            List<String> replacements = suggestion.getReplacements(text);
            if (replacements != null) {
              for (String replacement : replacements) {
                error.addReplacement(replacement);
              }
            }
          }
          errors.add(error);
          nextIndex = Math.max(nextIndex, startIndex + maxLength);
        }
      }

      // Go to the next non letter/digit character
      startIndex = Math.max(startIndex + 1, nextIndex);
      while ((startIndex < contents.length()) &&
             (((Character.isLetterOrDigit(contents.charAt(startIndex - 1))) &&
               (Character.isLetterOrDigit(contents.charAt(startIndex)))) ||
              (Character.isSpaceChar(contents.charAt(startIndex))) ||
              (Character.isWhitespace(contents.charAt(startIndex))) ||
              ("'.=|\"-)$*:;".indexOf(contents.charAt(startIndex)) >= 0))) {
        startIndex++;
      }
    }
    return result;
  }
}
