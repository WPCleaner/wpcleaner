/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a03x.a034;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementMagicWord;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 34 of check wikipedia project.
 * Error 34: Template programming element
 */
public class CheckErrorAlgorithm034 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm034() {
    super("Template programming element");
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
    if (analysis.isInNamespace(Namespace.TEMPLATE)) {
      return false;
    }

    // Check every position
    Page page = analysis.getPage();
    String contents = analysis.getContents();
    int maxLen = contents.length();
    boolean result = false;
    int currentIndex = 0;
    while (currentIndex < maxLen) {
      int nextIndex = currentIndex;
      if (contents.startsWith("__", currentIndex)) {
        nextIndex = currentIndex + 2;
        PageElementMagicWord magicWord = analysis.isInMagicWord(currentIndex);
        if ((magicWord != null) && (magicWord.getBeginIndex() == currentIndex)) {
          nextIndex = magicWord.getEndIndex();
          String magicWordName = magicWord.getMagicWord().getName();
          if (MagicWord.FORCE_TOC.equals(magicWordName) ||
              MagicWord.INDEX.equals(magicWordName) ||
              MagicWord.NO_INDEX.equals(magicWordName) ||
              MagicWord.NO_NEW_SECTION_LINK.equals(magicWordName)) {
            result = true;
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, magicWord.getBeginIndex(), magicWord.getEndIndex());
            errorResult.addReplacement("");
            errors.add(errorResult);
          }
        }
      } else if (contents.startsWith("{{", currentIndex)) {
        boolean done = false;

        // Check for templates beginning with '{{{' instead of '{{'
        if (!done &&
            contents.startsWith("{{{", currentIndex)) {
          PageElementTemplate currentTemplate = analysis.isInTemplate(currentIndex);
          PageElementTemplate nextTemplate = analysis.isInTemplate(currentIndex + 1);
          if ((nextTemplate != null) &&
              (currentIndex + 1 == nextTemplate.getBeginIndex()) &&
              ((currentTemplate == null) ||
               (currentTemplate.getBeginIndex() < currentIndex - 1))) {
            result = true;
            done = true;
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, currentIndex + 3);
            errorResult.addReplacement("{{");
            errors.add(errorResult);
            nextIndex = currentIndex + 3;
          }
        }

        // Check for parameters
        if (!done) {
          PageElementParameter parameter = analysis.isInParameter(currentIndex);
          if ((parameter != null) &&
              (parameter.getBeginIndex() == currentIndex)) {
            result = true;
            done = true;
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, parameter.getBeginIndex(), parameter.getEndIndex());
            if (parameter.getParameterCount() == 1) {
              String value = parameter.getParameterValue(0);
              if (value != null) {
                errorResult.addReplacement(value);
              }
            }
            errors.add(errorResult);
            nextIndex = parameter.getEndIndex();
          }
        }

        // Check for functions
        if (!done) {
          PageElementFunction function = analysis.isInFunction(currentIndex);
          if ((function != null) &&
              (function.getBeginIndex() == currentIndex)) {
            MagicWord magicWord = function.getMagicWord();
            String magicWordName = magicWord.getName();
            boolean isOk = false;
            ErrorLevel errorLevel = ErrorLevel.ERROR;
            if (MagicWord.DEFAULT_SORT.equals(magicWordName) ||
                MagicWord.FORMAT_NUM.equals(magicWordName) ||
                MagicWord.DISPLAY_TITLE.equals(magicWordName)) {
              isOk = true;
            }
            if (!isOk &&
                MagicWord.TAG.equals(magicWordName) &&
                (function.getParameterCount() > 0) &&
                (PageElementTag.TAG_WIKI_REF.equals(function.getParameterValue(0)))) {
              isOk = true;
            }
            if (!isOk) {
              if (MagicWord.INVOKE.equals(magicWordName) ||
                  MagicWord.SAFE_SUBST.equals(magicWordName) ||
                  MagicWord.SUBST.equals(magicWordName)) {
                errorLevel = ErrorLevel.WARNING;
              }
            }
            if (!isOk) {
              result = true;
              done = true;
              if (errors == null) {
                return true;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, function.getBeginIndex(), function.getEndIndex(), errorLevel);
              if (MagicWord.PAGE_NAME.equals(magicWordName)) {
                errorResult.addReplacement(page.getTitle());
              }
              if (MagicWord.IF_EXPR.equals(magicWordName)) {
                for (int param = 1; param < function.getParameterCount(); param++) {
                  errorResult.addReplacement(function.getParameterValue(param));
                }
              }
              if ((analysis.isInTag(currentIndex, PageElementTag.TAG_WIKI_GALLERY) == null) &&
                  (analysis.isInTag(currentIndex, PageElementTag.TAG_WIKI_INCLUDEONLY) == null) &&
                  (analysis.isInTag(currentIndex, PageElementTag.TAG_WIKI_REF) == null) &&
                  (analysis.isInTag(currentIndex, PageElementTag.TAG_WIKI_TIMELINE) == null) &&
                  (!MagicWord.INVOKE.equals(magicWordName)) &&
                  (!MagicWord.SUBST.equals(magicWordName)) &&
                  (!MagicWord.SAFE_SUBST.equals(magicWordName))) {
                errorResult.addReplacement(
                    "{{subst:" +
                    contents.substring(function.getBeginIndex() + 2, function.getEndIndex()));
              }
              errors.add(errorResult);
              nextIndex = function.getEndIndex();
            } else {
              nextIndex = currentIndex + 2;
            }
          }
        }
      }
      currentIndex = Math.max(nextIndex, currentIndex  + 1);
    }

    return result;
  }
}
