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

    // Retrieve possible abreviations before <ref> tag
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
    for (int i = 0; i < tags.size(); i++) {
      PageElementTag tag = tags.get(i);
      if ((tag != null) &&
          (tag.isFullTag() || !tag.isEndTag())) {

        // Remove possible whitespace
        int tmpIndex = tag.getBeginIndex() - 1;
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
            if (!abbreviationFound) {
              return true;
            }
          } else {
            if (!abbreviationFound) {
              result = true;
            }
            String allPunctuations = "";
            while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == punctuation)) {
              allPunctuations = contents.charAt(tmpIndex) + allPunctuations;
              tmpIndex--;
            }
            ArrayList<PageElementTag> tagList = new ArrayList<PageElementTag>();
            tagList.add(tag);
            boolean tryNext = true;
            while (tryNext) {
              PageElementTag lastTag = tagList.get(tagList.size() - 1);
              if (lastTag.getMatchingTag() != null) {
                lastTag = lastTag.getMatchingTag();
              }
              int lastTagIndex = i;
              while ((lastTagIndex < tags.size()) &&
                     (tags.get(lastTagIndex) != lastTag)) {
                lastTagIndex++;
              }
              PageElementTag nextTag = (lastTagIndex + 1 < tags.size()) ?
                  tags.get(lastTagIndex + 1) : null;
              if (nextTag == null) {
                tryNext = false;
              } else {
                boolean separatorFound = false;
                int endIndex = lastTag.getEndIndex();
                while ((endIndex < nextTag.getBeginIndex()) && (tryNext)) {
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
                  i = lastTagIndex;
                }
              }
            }
            PageElementTag lastTag = tagList.get(tagList.size() - 1);
            if (lastTag.getMatchingTag() != null) {
              lastTag = lastTag.getMatchingTag();
            }
            int endIndex = lastTag.getEndIndex();
            while ((endIndex < contents.length()) && (contents.charAt(endIndex) == punctuation)) {
              endIndex++;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), tmpIndex + 1, endIndex,
                abbreviationFound ? CheckErrorResult.ErrorLevel.CORRECT : CheckErrorResult.ErrorLevel.ERROR);
            String tagText = "";
            for (int j = 0; j < tagList.size(); j++) {
              if (j > 0) {
                tagText += separator; 
              }
              PageElementTag beginTag = tagList.get(j);
              PageElementTag endTag = beginTag;
              if (endTag.getMatchingTag() != null) {
                endTag = endTag.getMatchingTag();
              }
              tagText += contents.substring(
                  beginTag.getBeginIndex(),
                  endTag.getEndIndex());
            }
            if (tagList.size() > 1) {
              errorResult.addReplacement(
                  tagText + allPunctuations,
                  "<ref>...</ref>" + separator + "..." + separator + "<ref>...</ref>" + allPunctuations);
            } else {
              errorResult.addReplacement(
                  tagText + allPunctuations,
                  "<ref>...</ref>" + allPunctuations);
            }
            if ((endIndex > lastTag.getEndIndex()) &&
                (endIndex - lastTag.getEndIndex() != allPunctuations.length())) {
              String afterTag = contents.substring(lastTag.getEndIndex(), endIndex);
              if (tagList.size() > 1) {
                errorResult.addReplacement(
                    tagText + afterTag,
                    "<ref>...</ref>" + separator + "..." + separator + "<ref>...</ref>" + afterTag);
              } else {
                errorResult.addReplacement(
                    tagText + afterTag,
                    "<ref>...</ref>" + afterTag);
              }
            }
            errors.add(errorResult);
          }
        }
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
