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

      // Ignore in comments
      if (errorFound) {
        if (analysis.isInComment(currentIndex) != null) {
          errorFound = false;
        }
      }

      // Ignore if part of an unbalanced title 
      if (errorFound) {
        PageElementTitle title = analysis.isInTitle(currentIndex);
        if ((title != null)  && (title.getSecondLevel() <= title.getFirstLevel())) {
          errorFound = false;
        }
      }

      // Ignore in some tags
      if (errorFound) {
        if ((analysis.getSurroundingTag(PageElementTag.TAG_HTML_CODE, currentIndex) != null) ||
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

      // Functions used instead of some tags
      if (errorFound) {
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

      // Ignore if it's a template parameter "=" between parameter name and value
      if (errorFound && (tmpIndex == currentIndex + 1)) {
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

      // Ignore "=" at the end of external links
      if (errorFound) {
        PageElementExternalLink link = analysis.isInExternalLink(currentIndex);
        if ((link != null) && !link.hasSquare()) {
          errorFound = false;
        }
      }

      // Compute line beginning
      int beginLine = currentIndex;
      if (errorFound) {
        while ((beginLine > 0) && (contents.charAt(beginLine - 1) != '\n')) {
          beginLine--;
        }
      }

      // Ignore in tables
      if (errorFound) {
        if ((beginLine > 0) && (contents.charAt(beginLine) == '|')) {
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
            analysis, beginLine, tmpIndex);

        // Suggest possible replacements
        if (contents.charAt(beginLine) == '=') {
          int equalsBefore = beginLine;
          while ((equalsBefore < tmpIndex) && (contents.charAt(equalsBefore) == '=')) {
            equalsBefore++;
          }
          int equalsAfter = tmpIndex;
          while ((equalsAfter > beginLine) && (contents.charAt(equalsAfter - 1) == '=')) {
            equalsAfter--;
          }
          if (equalsBefore - beginLine != tmpIndex - equalsAfter) {
            errorResult.addReplacement(
                contents.substring(beginLine, equalsAfter) +
                contents.substring(beginLine, equalsBefore));
            errorResult.addReplacement(
                contents.substring(equalsAfter, tmpIndex) +
                contents.substring(equalsBefore, tmpIndex));
          } else {
            int extraBefore = equalsBefore;
            while ((extraBefore < tmpIndex) && (contents.charAt(extraBefore) == ' ')) {
              extraBefore++;
            }
            boolean extraBeforeFound = false;
            while ((extraBefore < tmpIndex) && (contents.charAt(extraBefore) == '=')) {
              extraBefore++;
              extraBeforeFound = true;
            }
            if (!extraBeforeFound) {
              extraBefore = equalsBefore;
            }
            int extraAfter = equalsAfter;
            while ((extraAfter > beginLine) && (contents.charAt(extraAfter - 1) == ' ')) {
              extraAfter--;
            }
            boolean extraAfterFound = false;
            while ((extraAfter > beginLine) && (contents.charAt(extraAfter - 1) == '=')) {
              extraAfter--;
              extraAfterFound = true;
            }
            if (!extraAfterFound) {
              extraAfter = equalsAfter;
            }
            if (extraBeforeFound || extraAfterFound && (extraAfter > extraBefore)) {
              errorResult.addReplacement(
                  contents.substring(beginLine, equalsBefore) +
                  contents.substring(extraBefore, extraAfter) +
                  contents.substring(equalsAfter, tmpIndex));
            }
          }
        }

        errors.add(errorResult);
      }

      currentIndex = nextIndex;
    }

    return result;
  }
}
