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

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TagBlock;


/**
 * Algorithm for analyzing error 67 of check wikipedia project.
 * Error 67: Reference after punctuation.
 */
public class CheckErrorAlgorithm067 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm067() {
    super("Reference after punctuation");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Retrieve possible abreviations before <ref> tag
    String abbreviations = page.getWikipedia().getCheckWikiProperty(
        "abbreviations", 67, true, false, false);
    String[] abbreviationsList = null;
    if (abbreviations != null) {
      abbreviationsList = page.getWikipedia().convertPropertyToStringArray(abbreviations);
    }

    // Analyze from the begining
    int startIndex = 0;
    boolean result = false;
    while ((startIndex >= 0) && (startIndex < contents.length())) {
      if (contents.charAt(startIndex) == '<') {
        TagBlock tag = findNextTag(page, contents, "ref", startIndex);
        if (tag != null) {
          startIndex = contents.indexOf('<', tag.getEndTagEndIndex());
          int tmpIndex = tag.getStartTagBeginIndex() - 1;

          // Remove possible whitespaces
          while ((tmpIndex >= 0) && (Character.isWhitespace(contents.charAt(tmpIndex)))) {
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

          // Check for possible abbreviations
          boolean abbreviationFound = false;
          if ((punctuationFound) && (abbreviationsList != null)) {
            for (String abbreviation : abbreviationsList) {
              if (abbreviation != null) {
                if (contents.startsWith(abbreviation, tmpIndex - abbreviation.length() + 1)) {
                  abbreviationFound = true;
                }
              }
            }
          }

          // Punctuation found
          if (punctuationFound) {
            if (errors == null) {
              return true;
            }
            result = true;
            String allPunctuations = "";
            while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == punctuation)) {
              allPunctuations = contents.charAt(tmpIndex) + allPunctuations;
              tmpIndex--;
            }
            int endIndex = tag.getEndTagEndIndex() + 1;
            while ((endIndex < contents.length()) && (contents.charAt(endIndex) == punctuation)) {
              endIndex++;
            }
            CheckErrorResult errorResult = new CheckErrorResult(
                getShortDescription(), tmpIndex + 1, endIndex,
                abbreviationFound ? CheckErrorResult.ErrorLevel.CORRECT : CheckErrorResult.ErrorLevel.ERROR);
            String tagText = contents.substring(tag.getStartTagBeginIndex(), tag.getEndTagEndIndex() + 1);
            errorResult.addReplacement(
                tagText + allPunctuations,
                "<ref>...</ref>" + allPunctuations);
            if (endIndex > tag.getEndTagEndIndex() + 1) {
              errorResult.addReplacement(
                  tagText + allPunctuations + contents.substring(tag.getEndTagEndIndex() + 1, endIndex),
                  "<ref>...</ref>" + allPunctuations + contents.substring(tag.getEndTagEndIndex() + 1, endIndex));
            }
            errors.add(errorResult);
          }
        } else {
          startIndex = contents.indexOf('<', startIndex + 1);
        }
      } else {
        startIndex = contents.indexOf('<', startIndex + 1);
      }
    }
    return result;
  }
}
