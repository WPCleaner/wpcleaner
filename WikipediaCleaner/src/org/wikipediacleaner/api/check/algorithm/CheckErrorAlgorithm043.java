/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 43 of check wikipedia project.
 * Error 43: Template not correct end
 */
public class CheckErrorAlgorithm043 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm043() {
    super("Template not correct end");
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

    // Analyze contents from the beginning
    String contents = pageAnalysis.getContents();
    int maxLength = contents.length();
    int currentIndex = contents.indexOf("{{");
    boolean result = false;
    while (currentIndex >= 0) {
      boolean shouldCount = true;
      if ((pageAnalysis.isInComment(currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null) ||
          (pageAnalysis.isInCategory(currentIndex) != null) ||
          (pageAnalysis.isInTag(currentIndex) != null)) {
        shouldCount = false;
      }
      if (shouldCount) {
        PageElementTemplate template = pageAnalysis.isInTemplate(currentIndex + 2);
        if ((template != null) &&
            ((template.getBeginIndex() == currentIndex) ||
             (template.getBeginIndex() == currentIndex + 1))) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementFunction function = pageAnalysis.isInFunction(currentIndex + 2);
        if ((function != null) &&
            ((function.getBeginIndex() == currentIndex) ||
             (function.getBeginIndex() == currentIndex + 1))) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if ((currentIndex + 2 < maxLength) &&
            (contents.charAt(currentIndex + 2) == '{')) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        
        // Check if there is a potential end
        int tmpIndex = currentIndex + 2;
        boolean errorReported = false;
        boolean finished = false;
        while (!finished && (tmpIndex < maxLength)) {
          char tmpChar = contents.charAt(tmpIndex);
          if ((tmpChar == '\n') ||
              (tmpChar == '[') ||
              (tmpChar == '{')) {
            finished = true;
          } else if (tmpChar == '}') {
            if ((tmpIndex + 1 >= maxLength) ||
                (contents.charAt(tmpIndex + 1) != '}')) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), currentIndex, tmpIndex + 1);
              errorResult.addReplacement(contents.substring(currentIndex, tmpIndex + 1) + "}");
  
              // Check if the situation is something like [[http://....] (replacement: [http://....])
              List<String> protocols = PageElementExternalLink.getProtocols();
              boolean protocolFound = false;
              for (String protocol : protocols) {
                if (contents.startsWith(protocol, currentIndex + 2)) {
                  protocolFound = true;
                }
              }
              if (protocolFound) {
                errorResult.addReplacement(contents.substring(currentIndex + 1, tmpIndex + 1));
              }
  
              errors.add(errorResult);
              result = true;
            }
            errorReported = true;
            finished = true;
          } else if (tmpChar == ']') {
            int lastChar = tmpIndex;
            if ((lastChar + 1 < maxLength) && (contents.charAt(lastChar + 1) == ']')) {
              lastChar++;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), currentIndex, lastChar + 1);
            errorResult.addReplacement(contents.substring(currentIndex, tmpIndex) + "}}");
            errorResult.addReplacement("[[" + contents.substring(currentIndex + 2, tmpIndex) + "]]");
            errors.add(errorResult);
            result = true;
            errorReported = true;
            finished = true;
          }
          tmpIndex++;
        }

        // Default
        if (!errorReported) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), currentIndex, currentIndex + 2);
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
          result = true;
        }
      }
      currentIndex = contents.indexOf("{{", currentIndex + 2);
    }

    return result;
  }
}
