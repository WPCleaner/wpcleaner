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
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
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
    Iterator<PageElementComment> itComments = pageAnalysis.getComments().iterator();
    PageElementComment currentComment = itComments.hasNext() ? itComments.next() : null;
    Iterator<PageElementInternalLink> itInternalLink = pageAnalysis.getInternalLinks().iterator();
    PageElementInternalLink currentInternalLink = itInternalLink.hasNext() ? itInternalLink.next() : null;
    Iterator<PageElementExternalLink> itExternalLink = pageAnalysis.getExternalLinks().iterator();
    PageElementExternalLink currentExternalLink = itExternalLink.hasNext() ? itExternalLink.next() : null;
    Iterator<PageElementTemplate> itTemplate = pageAnalysis.getTemplates().iterator();
    PageElementTemplate currentTemplate = itTemplate.hasNext() ? itTemplate.next() : null;
    Iterator<PageElementInterwikiLink> itInterwikiLink = pageAnalysis.getInterwikiLinks().iterator();
    PageElementInterwikiLink currentInterwikiLink = itInterwikiLink.hasNext() ? itInterwikiLink.next() : null;
    Iterator<PageElementLanguageLink> itLanguageLink = pageAnalysis.getLanguageLinks().iterator();
    PageElementLanguageLink currentLanguageLink = itLanguageLink.hasNext() ? itLanguageLink.next() : null;
    Iterator<PageElementCategory> itCategory = pageAnalysis.getCategories().iterator();
    PageElementCategory currentCategory = itCategory.hasNext() ? itCategory.next() : null;

    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {

      // Test if the position is correct to check orthograph
      int nextIndex = startIndex + 1;
      boolean checkOrthograph = true;

      // If the position is inside a comment, go after
      if (checkOrthograph && (currentComment != null)) {
        while ((currentComment != null) &&
               (startIndex >= currentComment.getEndIndex())) {
          currentComment = itComments.hasNext() ? itComments.next() : null;
        }
        if ((currentComment != null) &&
            (startIndex >= currentComment.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentComment.getEndIndex());
        }
      }

      // If the position is inside an internal link, go after
      if (checkOrthograph && (currentInternalLink != null)) {
        while ((currentInternalLink != null) &&
               (startIndex >= currentInternalLink.getEndIndex())) {
          currentInternalLink = itInternalLink.hasNext() ? itInternalLink.next() : null;
        }
        if ((currentInternalLink != null) &&
            (startIndex > currentInternalLink.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentInternalLink.getEndIndex());
        }
      }

      // If the position is inside an external link, go after
      if (checkOrthograph && (currentExternalLink != null)) {
        while ((currentExternalLink != null) &&
               (startIndex >= currentExternalLink.getEndIndex())) {
          currentExternalLink = itExternalLink.hasNext() ? itExternalLink.next() : null;
        }
        if ((currentExternalLink != null) &&
            (startIndex >= currentExternalLink.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentExternalLink.getEndIndex());
        }
      }

      // If the position is inside a template, go after
      if (checkOrthograph && (currentTemplate != null)) {
        while ((currentTemplate != null) &&
               (startIndex >= currentTemplate.getEndIndex())) {
          currentTemplate = itTemplate.hasNext() ? itTemplate.next() : null;
        }
        if ((currentTemplate != null) &&
            (startIndex >= currentTemplate.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentTemplate.getEndIndex());
        }
      }

      // If the position is inside an interwiki link, go after
      if (checkOrthograph && (currentInterwikiLink != null)) {
        while ((currentInterwikiLink != null) &&
               (startIndex >= currentInterwikiLink.getEndIndex())) {
          currentInterwikiLink = itInterwikiLink.hasNext() ? itInterwikiLink.next() : null;
        }
        if ((currentInterwikiLink != null) &&
            (startIndex >= currentInterwikiLink.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentInterwikiLink.getEndIndex());
        }
      }

      // If the position is inside a language link, go after
      if (checkOrthograph && (currentLanguageLink != null)) {
        while ((currentLanguageLink != null) &&
               (startIndex >= currentLanguageLink.getEndIndex())) {
          currentLanguageLink = itLanguageLink.hasNext() ? itLanguageLink.next() : null;
        }
        if ((currentLanguageLink != null) &&
            (startIndex >= currentLanguageLink.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentLanguageLink.getEndIndex());
        }
      }

      // If the position is inside a category, go after
      if (checkOrthograph && (currentCategory != null)) {
        while ((currentCategory != null) &&
               (startIndex >= currentCategory.getEndIndex())) {
          currentCategory = itCategory.hasNext() ? itCategory.next() : null;
        }
        if ((currentCategory != null) &&
            (startIndex >= currentCategory.getBeginIndex())) {
          checkOrthograph = false;
          nextIndex = Math.max(nextIndex, currentCategory.getEndIndex());
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
