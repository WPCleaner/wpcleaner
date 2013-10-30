/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
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
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (pageAnalysis == null) {
      return false;
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
      int lastTagIndex = PageElementTag.groupTags(tags, firstTagIndex, contents, ",;.\'", separator);
      PageElementTag lastTag = tags.get(lastTagIndex);
      tagIndex = lastTagIndex + 1;

      // Remove possible whitespace characters after last reference
      int tmpIndex = lastTag.getEndIndex();
      boolean finished = false;
      while (!finished) {
        if (tmpIndex >= contents.length()) {
          finished = true;
        } else if (contents.charAt(tmpIndex) == '\n') {
          if ((tmpIndex + 1 < contents.length()) &&
              ((Character.isWhitespace(contents.charAt(tmpIndex + 1))) ||
               (contents.charAt(tmpIndex + 1) == '*') || // List
               (contents.charAt(tmpIndex + 1) == '#') || // List
               (contents.charAt(tmpIndex + 1) == ';') || // Definition
               (contents.charAt(tmpIndex + 1) == ':') || // Definition
               (contents.charAt(tmpIndex + 1) == '!'))) { // Table heading
            finished = true;
          } else {
            tmpIndex++;
          }
        } else if (Character.isWhitespace(contents.charAt(tmpIndex))) {
          tmpIndex++;
        } else {
          finished = true;
        }
      }

      // Check if next character is a punctuation
      boolean punctuationFound = false;
      char punctuation = ' ';
      if (tmpIndex < contents.length()) {
        punctuation = contents.charAt(tmpIndex);
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

        // Check if the punctuation after is multiple
        int firstPunctuationIndex = tmpIndex;
        while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == punctuation)) {
          tmpIndex++;
        }
        int beginIndex = firstTag.getBeginIndex();
        int endIndex = tmpIndex;
        String allPunctuations = contents.substring(firstPunctuationIndex, endIndex);

        // Construct list of tags
        String replace = PageElementTag.createListOfTags(
            tags, firstTagIndex, lastTagIndex, contents, separator);
        String textReplace = PageElementTag.createReducedListOfTags(
            tags, firstTagIndex, lastTagIndex, separator);

        // Check for possible punctuation before tags
        tmpIndex = beginIndex - 1;
        while ((tmpIndex >= 0) &&
               (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex--;
        }
        beginIndex = tmpIndex + 1;
        boolean punctuationFoundBefore = false;
        int punctuationBeforeIndex = tmpIndex;
        while ((tmpIndex >= 0) &&
               SpecialCharacters.isPunctuation(contents.charAt(tmpIndex))) {
          punctuationFoundBefore = true;
          tmpIndex--;
        }
        String punctuationBefore = contents.substring(tmpIndex + 1, punctuationBeforeIndex + 1);
        if (punctuationFoundBefore) {
          beginIndex = tmpIndex + 1;
        }

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), beginIndex, endIndex);
        errorResult.addReplacement(
            allPunctuations + replace,
            allPunctuations + textReplace);
        if (punctuationFoundBefore &&
            !allPunctuations.equals(punctuationBefore)) {
          errorResult.addReplacement(
              punctuationBefore + replace,
              punctuationBefore + textReplace);
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
        "separator",
        GT._("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"));
    return parameters;
  }
}