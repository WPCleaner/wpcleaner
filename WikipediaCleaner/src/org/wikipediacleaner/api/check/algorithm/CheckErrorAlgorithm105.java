/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 105 of check wikipedia project.
 * Error 105: Headline should start with "="
 */
public class CheckErrorAlgorithm105 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm105() {
    super("Headline should start with \"=\"");
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

    // Check every "=" at the end of a line
    boolean result = false;
    String contents = analysis.getContents();
    int maxLen = contents.length();
    int currentIndex = 0;
    while (currentIndex < maxLen) {
      int tmpIndex = currentIndex;
      boolean errorFound = false;
      while ((tmpIndex < maxLen) && (contents.charAt(tmpIndex) == '=')) {
        tmpIndex++;
      }
      if ((tmpIndex > currentIndex + 1) && // At least 2 "="
          (tmpIndex < maxLen) &&
          (contents.charAt(tmpIndex) == '\n')) {
        errorFound = true;
      }
      int nextIndex = Math.max(currentIndex + 1, tmpIndex + 1);

      // Check that it is indeed an error
      if (errorFound) {
        // Ignore in comments
        if (analysis.isInComment(currentIndex) != null) {
          errorFound = false;
        }
      }
      if (errorFound) {
        // Ignore if part of an unbalanced title 
        PageElementTitle title = analysis.isInTitle(currentIndex);
        if ((title != null)  && (title.getSecondLevel() >= title.getFirstLevel())) {
          errorFound = false;
        }
      }
      if (errorFound) {
        // Ignore in some tags
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_CODE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH_CHEM, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_TIMELINE, currentIndex) != null)) {
          errorFound = false;
        }
      }
      if (errorFound) {
        // Functions used instead of some tags
        PageElementFunction function = analysis.isInFunction(currentIndex);
        if ((function != null) &&
            (function.getMagicWord() != null) &&
            (MagicWord.TAG.equals(function.getMagicWord().getName()))) {
          int functionIndex = function.getBeginIndex();
          while ((functionIndex < contents.length()) &&
                 (contents.charAt(functionIndex) == '{')) {
            functionIndex++;
          }
          while ((functionIndex < contents.length()) &&
                 (Character.isWhitespace(contents.charAt(functionIndex)))) {
            functionIndex++;
          }
          if ((functionIndex < contents.length()) &&
              (contents.startsWith(function.getFunctionName(), functionIndex))) {
            functionIndex += function.getFunctionName().length();
            while ((functionIndex < contents.length()) &&
                   (Character.isWhitespace(contents.charAt(functionIndex)))) {
              functionIndex++;
            }
            while ((functionIndex < contents.length()) &&
                   (contents.charAt(functionIndex) == ':')) {
              functionIndex++;
            }
            while ((functionIndex < contents.length()) &&
                   (Character.isWhitespace(contents.charAt(functionIndex)))) {
              functionIndex++;
            }
            int endIndex = -1;
            int pipeIndex = contents.indexOf('|', functionIndex);
            if ((pipeIndex > 0) && ((endIndex < 0) || (pipeIndex < endIndex))) {
              endIndex = pipeIndex;
            }
            int curlyIndex = contents.indexOf('}', functionIndex);
            if ((curlyIndex > 0) && ((endIndex < 0) || (curlyIndex < endIndex))) {
              endIndex = curlyIndex;
            }
            if (endIndex > 0) {
              String tagName = contents.substring(functionIndex, endIndex);
              if (tagName.equalsIgnoreCase(PageElementTag.TAG_WIKI_TIMELINE)) {
                errorFound = false;
              }
            }
          }
        }
      }
      if (errorFound && (tmpIndex == currentIndex + 1)) {
        // Ignore if it's a template parameter "=" between parameter name and value
        PageElementTemplate template = analysis.isInTemplate(currentIndex);
        if (template != null) {
          Parameter param = template.getParameterAtIndex(tmpIndex);
          if (param != null) {
            int valueIndex = param.getValueStartIndex();
            if (valueIndex >= tmpIndex) {
              errorFound = false;
            }
          }
        }
      }
      if (errorFound) {
        // Ignore "=" at the end of external links
        PageElementExternalLink link = analysis.isInExternalLink(currentIndex);
        if ((link != null) && !link.hasSquare()) {
          errorFound = false;
        }
      }

      // Signal error
      if (errorFound) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, currentIndex, tmpIndex);
        errors.add(errorResult);
      }

      currentIndex = nextIndex;
    }

    return result;
  }
}
