/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 47 of check wikipedia project.
 * Error 47: Template not correct begin
 */
public class CheckErrorAlgorithm047 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm047() {
    super("Template not correct begin");
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

    // Analyze contents from the beginning
    String contents = analysis.getContents();
    int maxLength = contents.length();
    int currentIndex = contents.indexOf("}}");
    boolean result = false;
    while (currentIndex > 0) {
      boolean shouldCount = true;
      if ((analysis.isInComment(currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_GRAPH, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null) ||
          (analysis.isInCategory(currentIndex) != null) ||
          (analysis.isInTag(currentIndex) != null)) {
        shouldCount = false;
      }
      if (shouldCount) {
        PageElementTemplate template = analysis.isInTemplate(currentIndex);
        if ((template != null) && (template.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementFunction function = analysis.isInFunction(currentIndex);
        if ((function != null) && (function.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if ((currentIndex + 2 < maxLength) &&
            (contents.charAt(currentIndex + 2) == '}')) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        boolean errorReported = false;

        // Check if it's an external link
        PageElementExternalLink link = analysis.isInExternalLink(currentIndex);
        if ((link != null) &&
            link.hasSquare() && link.hasSecondSquare() &&
            (currentIndex < link.getBeginIndex() + link.getTextOffset())) {
          String text = link.getLink();
          text = text.replaceAll("\\{", "%7B");
          text = text.replaceAll("\\}", "%7D");
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              link.getBeginIndex(), link.getEndIndex(),
              ErrorLevel.WARNING);
          errorResult.addReplacement(
              PageElementExternalLink.createExternalLink(text, link.getDisplayedText()));
          errors.add(errorResult);
          errorReported = true;
          result = true;
        }

        // Check if there is a potential beginning
        if (!errorReported) {
          int tmpIndex = currentIndex - 1;
          boolean finished = false;
          while (!finished && tmpIndex >= 0) {
            char tmpChar = contents.charAt(tmpIndex);
            if ((tmpChar == '\n') ||
                (tmpChar == ']') ||
                (tmpChar == '}')) {
              finished = true;
            } else if (tmpChar == '{') {
              if ((tmpIndex == 0) || (contents.charAt(tmpIndex - 1) != '{')) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, tmpIndex, currentIndex + 2);
                errorResult.addReplacement("{" + contents.substring(tmpIndex, currentIndex + 2));
                errors.add(errorResult);
                result = true;
              }
              errorReported = true;
              finished = true;
            } else if (tmpChar == '[') {
              int firstChar = tmpIndex;
              if ((firstChar > 0) && (contents.charAt(firstChar - 1) == '[')) {
                firstChar--;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, firstChar, currentIndex + 2);
              errorResult.addReplacement("{{" + contents.substring(tmpIndex + 1, currentIndex + 2));
              errorResult.addReplacement("[[" + contents.substring(tmpIndex + 1, currentIndex) + "]]");
              errors.add(errorResult);
              errorReported = true;
              result = true;
              finished = true;
            }
            tmpIndex--;
          }
        }

        // Default
        if (!errorReported) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, currentIndex, currentIndex + 2);
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
          result = true;
        }
        if ((result == true) && (errors == null)) {
          return true;
        }
      }
      currentIndex = contents.indexOf("}}", currentIndex + 2);
    }

    return result;
  }
}
