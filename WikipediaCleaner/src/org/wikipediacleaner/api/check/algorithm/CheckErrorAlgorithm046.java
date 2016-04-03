/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 46 of check wikipedia project.
 * Error 46: Square brackets not correct begin
 */
public class CheckErrorAlgorithm046 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm046() {
    super("Square brackets not correct begin");
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
    int currentIndex = contents.indexOf("]]");
    boolean result = false;
    while (currentIndex > 0) {
      boolean shouldCount = true;
      if (shouldCount) {
        PageElementInternalLink link = analysis.isInInternalLink(currentIndex);
        if ((link != null) &&
            ((link.getEndIndex() == currentIndex + 2) ||
             (link.getEndIndex() == currentIndex + 3))) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementImage image = analysis.isInImage(currentIndex);
        if ((image != null) && (image.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementCategory category = analysis.isInCategory(currentIndex);
        if ((category != null) && (category.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementLanguageLink link = analysis.isInLanguageLink(currentIndex);
        if ((link != null) && (link.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementInterwikiLink link = analysis.isInInterwikiLink(currentIndex);
        if ((link != null) && (link.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount &&
          (analysis.isInComment(currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH_CHEM, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null) ||
          (analysis.isInTag(currentIndex) != null)) {
        shouldCount = false;
      }
      if (shouldCount) {
        PageElementTemplate template = analysis.isInTemplate(currentIndex - 1);
        if ((template != null) &&
            (template.getEndIndex() == currentIndex) &&
            (contents.startsWith("[[", template.getBeginIndex() - 2))) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementExternalLink link = analysis.isInExternalLink(currentIndex);
        if ((link != null) &&
            (link.getEndIndex() == currentIndex + 1)) {
          if ((link.getBeginIndex() == 0) ||
              (contents.charAt(link.getBeginIndex() - 1) != '[')) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, currentIndex + 2);
            errorResult.addReplacement("]");
            errors.add(errorResult);
          }
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check if there is a potential beginning
        int tmpIndex = currentIndex - 1;
        boolean errorReported = false;
        boolean finished = false;
        while (!finished && tmpIndex >= 0) {
          char tmpChar = contents.charAt(tmpIndex);
          if ((tmpChar == '\n') ||
              (tmpChar == ']') ||
              (tmpChar == '}')) {
            finished = true;
          } else if (tmpChar == '[') {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, tmpIndex, currentIndex + 2);

            // Check if the situation is something like [http://....]] (replacement: [http://....])
            boolean protocolFound = PageElementExternalLink.isPossibleProtocol(contents, tmpIndex + 1);
            if (protocolFound) {
              errorResult.addReplacement(contents.substring(tmpIndex, currentIndex + 1));
            }

            errorResult.addReplacement("[" + contents.substring(tmpIndex, currentIndex + 2));
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          } else if (tmpChar == '{') {
            int firstChar = tmpIndex;
            if ((firstChar > 0) && (contents.charAt(firstChar - 1) == '{')) {
              firstChar--;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, firstChar, currentIndex + 2);
            errorResult.addReplacement("[[" + contents.substring(tmpIndex + 1, currentIndex + 2));
            errorResult.addReplacement("{{" + contents.substring(tmpIndex + 1, currentIndex) + "}}");
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          }
          tmpIndex--;
        }

        // Default
        if (!errorReported) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, currentIndex, currentIndex + 2);
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      }
      currentIndex = contents.indexOf("]]", currentIndex + 2);
    }

    return result;
  }
}
