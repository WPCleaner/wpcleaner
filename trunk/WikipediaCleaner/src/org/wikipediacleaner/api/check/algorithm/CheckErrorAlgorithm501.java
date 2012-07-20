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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Suggestion;


/**
 * Algorithm for analyzing error 501 of check wikipedia project.
 * Error 501: Spelling and typography
 */
public class CheckErrorAlgorithm501 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm501() {
    super("Spelling and typography");
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
    if (!pageAnalysis.shouldCheckSpelling()) {
      return false;
    }
    Map<String, Suggestion> suggestions = pageAnalysis.getWPCConfiguration().getSuggestions();
    if ((suggestions == null) || (suggestions.isEmpty())) {
      return false;
    }

    // Initialize matchers
    String contents = pageAnalysis.getContents();
    Map<Suggestion, Matcher> matchersText = new HashMap<Suggestion, Matcher>(suggestions.size());
    Map<Suggestion, Matcher> matchersInternalLink = new HashMap<Suggestion, Matcher>();
    Map<Suggestion, Matcher> matchersTemplate = new HashMap<Suggestion, Matcher>();
    for (Suggestion suggestion : suggestions.values()) {
      Matcher matcher = suggestion.initMatcher(contents);
      if (suggestion.getPatternText().startsWith("\\[")) {
        matchersInternalLink.put(suggestion, matcher);
      } else if (suggestion.getPatternText().startsWith("\\{\\{")) {
        matchersTemplate.put(suggestion, matcher);
      } else {
        matchersText.put(suggestion, matcher);
      }
    }

    List<Suggestion> possibles = new ArrayList<Suggestion>();
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {

      // Test if the position is correct to check spelling
      int nextIndex = startIndex + 1;
      boolean checkSpelling = true;
      Map<Suggestion, Matcher> matchers = matchersText;

      // Check what kind of checking should be done
      char currentChar = contents.charAt(startIndex);
      if (currentChar == '[') {
        PageElementInternalLink link = pageAnalysis.isInInternalLink(startIndex);
        if ((link == null) || (link.getBeginIndex() != startIndex)) {
          checkSpelling = false;
        } else {
          matchers = matchersInternalLink;
        }
      } else if (currentChar == '{') {
        PageElementTemplate template = pageAnalysis.isInTemplate(startIndex);
        if ((template == null) || (template.getBeginIndex() != startIndex)) {
          checkSpelling = false;
        } else {
          matchers = matchersTemplate;
        }
      } else {
        // Check for special areas
        int tmp = pageAnalysis.getAreas().getEndArea(startIndex);
        if (tmp > startIndex) {
          checkSpelling = false;
          nextIndex = startIndex + 1;
        }

        // Check for template
        if (checkSpelling) {
          PageElementTemplate template = pageAnalysis.isInTemplate(startIndex);
          if (template != null) {
            checkSpelling = false;
          }
        }

        // Check for gallery tag
        if (checkSpelling) {
          PageElementTag galleryTag = pageAnalysis.getSurroundingTag(
              PageElementTag.TAG_WIKI_GALLERY, startIndex);
          if (galleryTag != null) {
            // TODO: Be more precise, analyze image descriptions
            checkSpelling = false;
            nextIndex = galleryTag.getCompleteEndIndex();
          }
        }

        // Check for math tag
        if (checkSpelling) {
          PageElementTag mathTag = pageAnalysis.getSurroundingTag(
              PageElementTag.TAG_WIKI_MATH, startIndex);
          if (mathTag != null) {
            checkSpelling = false;
            nextIndex = mathTag.getCompleteEndIndex();
          }
        }
      }

      // Check spelling
      if (checkSpelling) {
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
          boolean suggestionAdded = false;
          for (Suggestion suggestion : possibles) {
            String comment = suggestion.getComment();
            if (comment != null) {
              error.addPossibleAction(comment, new NullActionProvider());
            }
            List<String> replacements = suggestion.getReplacements(text);
            if (replacements != null) {
              for (String replacement : replacements) {
                if (!text.equals(replacement)) {
                  suggestionAdded = true;
                  error.addReplacement(replacement);
                }
              }
            }
          }
          if (suggestionAdded) {
            if (errors == null) {
              return true;
            }
            result = true;
            errors.add(error);
          }
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
