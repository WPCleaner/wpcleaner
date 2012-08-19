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
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 67 of check wikipedia project.
 * Error 67: Reference after punctuation.
 */
public class CheckErrorAlgorithm067 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm067() {
    super("Reference after punctuation");
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

    // Retrieve possible abbreviations before <ref> tag
    String abbreviations = getSpecificProperty(
        "abbreviations", true, false, false);
    String[] abbreviationsList = null;
    if (abbreviations != null) {
      abbreviationsList = EnumWikipedia.convertPropertyToStringArray(abbreviations);
    }

    // Retrieve separator between several <ref> tags
    String separator = getSpecificProperty(
        "separator", true, false, false);
    if (separator == null) {
      separator = "";
    }

    // Analyze from the beginning
    List<PageElementTag> tags = pageAnalysis.getTags(PageElementTag.TAG_WIKI_REF);
    if (tags == null) {
      return false;
    }
    boolean result = false;
    String contents = pageAnalysis.getContents();
    int tagIndex = 0;
    int maxTags = tags.size();
    while (tagIndex < maxTags) {

      // Group tags separated only by punctuation characters
      int firstTagIndex = tagIndex;
      PageElementTag firstTag = tags.get(firstTagIndex);
      int lastTagIndex = PageElementTag.groupTags(tags, firstTagIndex, contents, separator);
      PageElementTag lastTag = tags.get(lastTagIndex);
      tagIndex = lastTagIndex + 1;

      // Remove possible whitespace characters before first reference
      int tmpIndex = firstTag.getBeginIndex() - 1;
      while ((tmpIndex >= 0) &&
             (Character.isWhitespace(contents.charAt(tmpIndex)))) {
        tmpIndex--;
      }

      // Check if previous character is a punctuation
      boolean punctuationFound = false;
      char punctuation = ' ';
      if (tmpIndex >= 0) {
        punctuation = contents.charAt(tmpIndex);
        if (SpecialCharacters.isPunctuation(punctuation)) {
          punctuationFound = true;
        }
      }
      int beginIndex = tmpIndex;

      // Check for possible abbreviations before punctuation
      boolean abbreviationFound = false;
      if ((punctuationFound && (abbreviationsList != null))) {
        for (String abbreviation : abbreviationsList) {
          if (abbreviation != null) {
            if (contents.startsWith(abbreviation, tmpIndex - abbreviation.length() + 1)) {
              abbreviationFound = true;
            }
          }
        }
      }

      // Punctuation found
      if (punctuationFound && !abbreviationFound) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check if the punctuation before is multiple
        int lastPunctuationIndex = tmpIndex;
        while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == punctuation)) {
          tmpIndex--;
        }
        tmpIndex++;
        beginIndex = tmpIndex;
        String allPunctuations = contents.substring(tmpIndex, lastPunctuationIndex + 1);

        // Construct list of tags
        String replace = PageElementTag.createListOfTags(
            tags, firstTagIndex, lastTagIndex, contents, separator);
        String textReplace = PageElementTag.createReducedListOfTags(
            tags, firstTagIndex, lastTagIndex, separator);

        // Check for possible punctuation after tags
        tmpIndex = lastTag.getEndIndex();
        int endIndex = tmpIndex;
        while ((tmpIndex < contents.length()) &&
               (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        boolean punctuationFoundAfter = false;
        int punctuationAfterIndex = tmpIndex;
        while ((tmpIndex < contents.length()) &&
               SpecialCharacters.isPunctuation(contents.charAt(tmpIndex))) {
          punctuationFoundAfter = true;
          tmpIndex++;
        }
        String punctuationAfter = contents.substring(punctuationAfterIndex, tmpIndex);
        if (punctuationFoundAfter) {
          endIndex = tmpIndex;
        }

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), beginIndex, endIndex);
        errorResult.addReplacement(
            replace + allPunctuations,
            textReplace + allPunctuations);
        if (punctuationFoundAfter &&
            !allPunctuations.equals(punctuationAfter)) {
          errorResult.addReplacement(
              replace + punctuationAfter,
              textReplace + punctuationAfter);
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "abbreviations",
        GT._("A list of abbreviations that generate false positives when placed before {0}", "&lt;ref&gt;"));
    parameters.put(
        "separator",
        GT._("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"));
    return parameters;
  }
}
