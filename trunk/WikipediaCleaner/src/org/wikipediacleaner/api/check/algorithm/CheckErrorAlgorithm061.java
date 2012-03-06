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
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementTagFull;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 61 of check wikipedia project.
 * Error 61: Reference with punctuation
 */
public class CheckErrorAlgorithm061 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm061() {
    super("Reference before punctuation");
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

    // Retrieve separator between several <ref> tags
    String separator = getSpecificProperty(
        "separator", true, false, false);
    if (separator == null) {
      separator = "";
    }

    // Analyze from the begining
    int startIndex = 0;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      PageElementTagFull tag = PageContents.findNextTagFull(
          pageAnalysis.getPage(), contents, "ref", startIndex);
      if (tag != null) {
        startIndex = tag.getEndTagEndIndex();

        // Check for consecutive <ref> tags
        ArrayList<PageElementTagFull> tagList = new ArrayList<PageElementTagFull>();
        tagList.add(tag);
        boolean tryNext = true;
        while (tryNext) {
          int endIndex = tagList.get(tagList.size() - 1).getEndTagEndIndex();
          PageElementTagFull nextTag = PageContents.findNextTagFull(
              pageAnalysis.getPage(), contents, "ref", endIndex);
          if (nextTag == null) {
            tryNext = false;
          } else {
            boolean separatorFound = false;
            while ((endIndex < nextTag.getStartTagBeginIndex()) && (tryNext)) {
              if (!separatorFound && (contents.startsWith(separator, endIndex))) {
                separatorFound = true;
                endIndex += separator.length();
              } else if (!Character.isWhitespace(contents.charAt(endIndex)) &&
                         (contents.charAt(endIndex) != ',') &&
                         (contents.charAt(endIndex) != ';') &&
                         (contents.charAt(endIndex) != '\'')) {
                tryNext = false;
              } else {
                endIndex++;
              }
            }
            if (tryNext) {
              tagList.add(nextTag);
              startIndex = nextTag.getEndTagEndIndex();
            }
          }
        }
        PageElementTagFull lastTag = tagList.get(tagList.size() - 1);

        // Remove possible whitespaces
        int endIndex = lastTag.getEndTagEndIndex();
        while ((endIndex < contents.length()) && (Character.isWhitespace(contents.charAt(endIndex)))) {
          endIndex++;
        }

        // Check if next character is a punctuation
        boolean punctuationFound = false;
        char punctuation = ' ';
        if (endIndex < contents.length()) {
          punctuation = contents.charAt(endIndex);
          if (SpecialCharacters.isPunctuation(punctuation)) {
            punctuationFound = true;
          }
        }

        // Punctuation found
        if (punctuationFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          String allPunctuations = "";
          while ((endIndex < contents.length()) && (contents.charAt(endIndex) == punctuation)) {
            allPunctuations = allPunctuations + contents.charAt(endIndex);
            endIndex++;
          }
          int beginIndex = tag.getStartTagBeginIndex();
          while ((beginIndex > 0) &&
                 ((contents.charAt(beginIndex - 1) == ' ') ||
                  (contents.charAt(beginIndex - 1) == punctuation))) {
            beginIndex--;
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              beginIndex, endIndex,
              CheckErrorResult.ErrorLevel.ERROR);
          String tagText = "";
          for (int i = 0; i < tagList.size(); i++) {
            if (i > 0) {
              tagText += separator; 
            }
            tagText += contents.substring(
                tagList.get(i).getStartTagBeginIndex(),
                tagList.get(i).getEndTagEndIndex());
          }
          if (tagList.size() > 1) {
            errorResult.addReplacement(
                allPunctuations + tagText,
                allPunctuations + "<ref>...</ref>" + separator + "..." + separator + "<ref>...</ref>");
          } else {
            errorResult.addReplacement(
                allPunctuations + tagText,
                allPunctuations + "<ref>...</ref>");
          }
          errors.add(errorResult);
        }
      } else {
        startIndex = contents.length();
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
        "separator",
        GT._("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"));
    return parameters;
  }
}
