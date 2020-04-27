/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.contents.ContentsComment;
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
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
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

      // Check if previous character is a punctuation
      int tmpIndex = firstTag.getBeginIndex() - 1;
      String previousComment = "";
      boolean punctuationFoundBefore = false;
      boolean punctuationFoundBetween = false;
      char punctuation = ' ';
      if (firstTag.isComplete() || !firstTag.isEndTag()) {
        for (int currentTagIndex = firstTagIndex; currentTagIndex <= lastTagIndex; currentTagIndex++) {
          PageElementTag currentTag = tags.get(currentTagIndex);
          if ((currentTagIndex == firstTagIndex) ||
              (currentTag.isFullTag()) ||
              (!currentTag.isEndTag())) {
            int testIndex = currentTag.getBeginIndex() - 1;
            boolean done = false;
            while (!done) {
              done = true;
              if (testIndex >= 0) {
                char testChar = contents.charAt(testIndex);
                if (Character.isWhitespace(testChar)) {
                  testIndex--;
                  done = false;
                } else  if ((testChar == '>')) {
                  ContentsComment comment = analysis.isInComment(testIndex);
                  if ((comment != null) && (comment.getEndIndex() == testIndex + 1)) {
                    if (currentTagIndex == firstTagIndex) {
                      previousComment += contents.substring(comment.getBeginIndex(), comment.getEndIndex());
                    }
                    testIndex = comment.getBeginIndex() - 1;
                    done = false;
                  }
                }
              }
            }
            if (currentTagIndex == firstTagIndex) {
              tmpIndex = testIndex;
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

          // Search for general abbreviations
          int beginRefIndex = firstTag.getBeginIndex();
          int firstAbbreviationIndex = beginRefIndex;
          List<String[]> generalAbbreviationFound = new ArrayList<String[]>();
          if ((punctuationFoundBefore && (generalAbbreviations != null))) {
            for (String[] abbreviation : generalAbbreviations) {
              if ((abbreviation != null) &&
                  (abbreviation.length > 2) &&
                  (abbreviation[0] != null)) {
                String abbreviationText = abbreviation[0];
                int abbreviationStart = tmpIndex - abbreviationText.length() + 1;
                if (contents.startsWith(abbreviationText, abbreviationStart)) {
                  generalAbbreviationFound.add(abbreviation);
                  firstAbbreviationIndex = Math.min(firstAbbreviationIndex, abbreviationStart);
                }
              }
            }
          }

          // Check if the punctuation before is multiple
          int lastPunctuationIndex = tmpIndex;
          while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == punctuation)) {
            tmpIndex--;
          }
          tmpIndex++;
          beginIndex = tmpIndex;
          String allPunctuations = contents.substring(tmpIndex, lastPunctuationIndex + 1);
          while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == ' ')) {
            tmpIndex--;
          }
          String moveablePrefix = contents.substring(tmpIndex, beginIndex);
          String prefix = "";
          if (firstAbbreviationIndex < tmpIndex) {
            prefix = contents.substring(firstAbbreviationIndex, tmpIndex);
          }
          beginIndex = Math.min(tmpIndex, firstAbbreviationIndex);
  
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
          if (allPunctuations.equals(".") && !punctuationFoundAfter) {
            tmpIndex = endIndex;
            while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
              tmpIndex++;
            }
            if (contents.startsWith("\n\n", tmpIndex) ||
                contents.startsWith("\n*", tmpIndex)) {
              if (previousComment.isEmpty()) {
                automatic = true;
              }
            }
          }
          for (String[] generalAbbreviation : generalAbbreviationFound) {
            if ((generalAbbreviation.length > 2)) {
              String abbreviation = generalAbbreviation[2];
              String meaning = "";
              if (generalAbbreviation[1].length() > 0) {
                meaning = " (" + generalAbbreviation[1] + ")";
              }
              errorResult.addReplacement(
                  abbreviation + previousComment + replace + punctuationAfter,
                  abbreviation + textReplace + punctuationAfter + meaning);
            }
          }
          errorResult.addReplacement(
              prefix + previousComment + replace + moveablePrefix + allPunctuations,
              prefix + textReplace + moveablePrefix + allPunctuations, automatic);
          if (punctuationFoundAfter &&
              !allPunctuations.equals(punctuationAfter)) {
            errorResult.addReplacement(
                prefix + previousComment + replace + moveablePrefix + punctuationAfter,
                prefix + textReplace + moveablePrefix + punctuationAfter);
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
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        analysis.getPage().isInUserNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Abbreviations that can create false positives */
  private static final String PARAMETER_ABBREVIATIONS = "abbreviations";

  /** Separator between consecutive tags */
  private static final String PARAMETER_SEPARATOR = "separator";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_ABBREVIATIONS, true, false, false);
    abbreviationsList.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        abbreviationsList.addAll(tmpList);
      }
    }

    separator = getSpecificProperty(PARAMETER_SEPARATOR, true, false, false);
    if (separator == null) {
      separator = "";
    }

    generalAbbreviations.clear();
    List<String> tmpGeneralAbbreviations = getWPCConfiguration().getStringList(
        WPCConfigurationStringList.ABBREVIATIONS);
    if ((tmpGeneralAbbreviations != null) && (tmpGeneralAbbreviations.size() > 0)) {
      for (String tmpAbbr : tmpGeneralAbbreviations) {
        int pipeIndex1 = tmpAbbr.indexOf('|');
        if (pipeIndex1 > 0) {
          int pipeIndex2 = tmpAbbr.indexOf('|', pipeIndex1 + 1);
          if (pipeIndex2 > 0) {
            String[] abbreviation = new String[3];
            abbreviation[0] = tmpAbbr.substring(0, pipeIndex1).trim();
            abbreviation[1] = tmpAbbr.substring(pipeIndex1 + 1, pipeIndex2).trim();
            abbreviation[2] = tmpAbbr.substring(pipeIndex2 + 1).trim();
            generalAbbreviations.add(abbreviation);
          }
        }
      }
    }
  }

  /** Abbreviations that can create false positives */
  private final List<String> abbreviationsList = new ArrayList<>();

  /** Separator between consecutive tags */
  private String separator = "";

  /** Abbreviations that can create false positives */
  private final List<String[]> generalAbbreviations = new ArrayList<>();

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        PARAMETER_ABBREVIATIONS,
        GT._T("A list of abbreviations that generate false positives when placed before {0}", "&lt;ref&gt;"));
    parameters.put(
        PARAMETER_SEPARATOR,
        GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"));
    return parameters;
  }
}
