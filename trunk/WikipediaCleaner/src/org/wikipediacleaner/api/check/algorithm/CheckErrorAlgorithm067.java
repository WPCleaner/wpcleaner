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
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.WPCConfiguration;
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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Retrieve possible abbreviations before <ref> tag
    String abbreviations = getSpecificProperty(
        "abbreviations", true, false, false);
    List<String> abbreviationsList = null;
    if (abbreviations != null) {
      abbreviationsList = WPCConfiguration.convertPropertyToStringList(abbreviations);
    }

    // Retrieve separator between several <ref> tags
    String separator = getSpecificProperty(
        "separator", true, false, false);
    if (separator == null) {
      separator = "";
    }

    // Analyze from the beginning
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_WIKI_REF);
    if (tags == null) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    int tagIndex = 0;
    int maxTags = tags.size();
    while (tagIndex < maxTags) {

      // Group tags separated only by punctuation characters
      int firstTagIndex = tagIndex;
      PageElementTag firstTag = tags.get(firstTagIndex);
      int lastTagIndex = PageElementTag.groupTags(tags, firstTagIndex, contents, ",;.\'", separator);
      PageElementTag lastTag = tags.get(lastTagIndex);
      tagIndex = lastTagIndex + 1;

      // Remove possible whitespace characters before first reference
      int tmpIndex = firstTag.getBeginIndex() - 1;
      while ((tmpIndex >= 0) &&
             (Character.isWhitespace(contents.charAt(tmpIndex)))) {
        tmpIndex--;
      }

      // Check if previous character is a punctuation
      boolean punctuationFoundBefore = false;
      boolean punctuationFoundBetween = false;
      char punctuation = ' ';
      for (int currentTagIndex = firstTagIndex; currentTagIndex <= lastTagIndex; currentTagIndex++) {
        PageElementTag currentTag = tags.get(currentTagIndex);
        int testIndex = currentTag.getBeginIndex() - 1;
        while ((testIndex >= 0) && (Character.isWhitespace(contents.charAt(testIndex)))) {
          testIndex--;
        }
        if (testIndex >= 0) {
          char currentPunctuation = contents.charAt(testIndex);
          if (SpecialCharacters.isPunctuation(currentPunctuation)) {
            boolean punctuationFound = true;
            if (punctuation == ';') {
              int punctuationIndex = testIndex;
              testIndex--;
              while((testIndex >= 0) && (Character.isLetterOrDigit(contents.charAt(testIndex)))) {
                testIndex--;
              }
              if ((testIndex >= 0) && (contents.charAt(testIndex) == '&')) {
                String name = contents.substring(testIndex + 1, punctuationIndex);
                for (HtmlCharacters htmlCharacter : HtmlCharacters.values()) {
                  if (name.equals(htmlCharacter.getName())) {
                    punctuationFound = false;
                  }
                }
              }
            }
            if (punctuationFound) {
              if (currentTagIndex == firstTagIndex) {
                punctuationFoundBefore = true;
                punctuation = currentPunctuation;
              } else {
                punctuationFoundBetween = true;
              }
            }
          }
        }
      }
      int beginIndex = tmpIndex;

      // Check for possible abbreviations before punctuation
      boolean abbreviationFound = false;
      if ((punctuationFoundBefore && (abbreviationsList != null))) {
        for (String abbreviation : abbreviationsList) {
          if (abbreviation != null) {
            if (contents.startsWith(abbreviation, tmpIndex - abbreviation.length() + 1)) {
              abbreviationFound = true;
            }
          }
        }
      }

      // Punctuation found
      if ((punctuationFoundBefore && !abbreviationFound) || punctuationFoundBetween) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Construct list of tags
        String replace = PageElementTag.createListOfTags(
            tags, firstTagIndex, lastTagIndex, contents, separator);
        String textReplace = PageElementTag.createReducedListOfTags(
            tags, firstTagIndex, lastTagIndex, separator);

        if (punctuationFoundBefore && !abbreviationFound) {
          // Check if the punctuation before is multiple
          int lastPunctuationIndex = tmpIndex;
          while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == punctuation)) {
            tmpIndex--;
          }
          tmpIndex++;
          beginIndex = tmpIndex;
          String allPunctuations = contents.substring(tmpIndex, lastPunctuationIndex + 1);
  
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
              analysis, beginIndex, endIndex);
          boolean automatic = false;
          if (allPunctuations.equals(".") &&
              (!punctuationFoundAfter || punctuationAfter.equals("."))) {
            tmpIndex = endIndex;
            while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
              tmpIndex++;
            }
            if (contents.startsWith("\n\n", tmpIndex) ||
                contents.startsWith("\n*", tmpIndex)) {
              automatic = true;
            }
          }
          errorResult.addReplacement(
              replace + allPunctuations,
              textReplace + allPunctuations, automatic);
          if (punctuationFoundAfter &&
              !allPunctuations.equals(punctuationAfter)) {
            errorResult.addReplacement(
                replace + punctuationAfter,
                textReplace + punctuationAfter);
          }
          errors.add(errorResult);

        } else {

          // Create error
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, firstTag.getBeginIndex(), lastTag.getEndIndex());
          errorResult.addReplacement(replace, textReplace);
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
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

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
