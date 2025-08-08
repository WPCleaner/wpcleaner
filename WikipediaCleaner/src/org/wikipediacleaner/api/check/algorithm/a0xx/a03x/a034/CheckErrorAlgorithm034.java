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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementMagicWord;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.magicword.FunctionMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.MagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.SimpleMagicWordType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 34 of check wikipedia project.
 * Error 34: Template programming element
 */
@SuppressWarnings("unused")
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
    String contents = analysis.getContents();
    int maxLen = contents.length();
    boolean result = false;
    Progress progress = new Progress();
    while (progress.currentIndex < maxLen) {
      int currentIndex = progress.currentIndex;
      result = analyzeIndex(analysis, progress, errors) || result;
      progress.currentIndex = Math.max(progress.currentIndex, currentIndex + 1);
    }

    return result;
  }

  private boolean analyzeIndex(
      PageAnalysis analysis,
      Progress progress,
      Collection<CheckErrorResult> errors) {
    String contents = analysis.getContents();
    if (contents.startsWith("__", progress.currentIndex)) {
      return analyzeUnderscores(analysis, progress, errors);
    }
    if (contents.startsWith("{{", progress.currentIndex)) {
      return analyzeCurlyBraces(analysis, progress, errors);
    }
    return false;
  }

  private boolean analyzeCurlyBraces(
      PageAnalysis analysis,
      Progress progress,
      Collection<CheckErrorResult> errors) {
    String contents = analysis.getContents();

    // Check for templates beginning with '{{{' instead of '{{'
    if (contents.startsWith("{{{", progress.currentIndex)) {
      PageElementTemplate currentTemplate = analysis.isInTemplate(progress.currentIndex);
      PageElementTemplate nextTemplate = analysis.isInTemplate(progress.currentIndex + 1);
      if ((nextTemplate != null) &&
          (progress.currentIndex + 1 == nextTemplate.getBeginIndex()) &&
          ((currentTemplate == null) ||
              (currentTemplate.getBeginIndex() < progress.currentIndex - 1))) {
        if (errors == null) {
          return true;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, progress.currentIndex, progress.currentIndex + 3);
        errorResult.addReplacement("{{");
        errors.add(errorResult);
        progress.currentIndex += 3;
        return true;
      }
    }

    // Check for parameters
    PageElementParameter parameter = analysis.isInParameter(progress.currentIndex);
    if ((parameter != null) &&
        (parameter.getBeginIndex() == progress.currentIndex)) {
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
      progress.currentIndex = parameter.getEndIndex();
      return true;
    }

    // Check for functions
    PageElementFunction function = analysis.isInFunction(progress.currentIndex);
    if (function == null || function.getBeginIndex() != progress.currentIndex) {
      return false;
    }
    MagicWord magicWord = function.getMagicWord();
    MagicWordType magicWordType = magicWord.getType();
    ErrorLevel errorLevel = ErrorLevel.ERROR;
    int currentIndex = progress.currentIndex;
    progress.currentIndex += 2;
    if (FunctionMagicWordType.DEFAULT_SORT.equals(magicWordType) ||
        FunctionMagicWordType.FORMAT_NUM.equals(magicWordType) ||
        FunctionMagicWordType.DISPLAY_TITLE.equals(magicWordType)) {
      return false;
    }
    if (FunctionMagicWordType.TAG.equals(magicWordType) &&
        (function.getParameterCount() > 0) &&
        (WikiTagType.REF.isPossibleName(function.getParameterValue(0)))) {
      return false;
    }
    if (FunctionMagicWordType.INVOKE.equals(magicWordType) ||
        FunctionMagicWordType.SAFE_SUBST.equals(magicWordType) ||
        FunctionMagicWordType.SUBST.equals(magicWordType)) {
      errorLevel = ErrorLevel.WARNING;
    }
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, function.getBeginIndex(), function.getEndIndex(), errorLevel);
    if (FunctionMagicWordType.PAGE_NAME.equals(magicWordType)) {
      errorResult.addReplacement(analysis.getPage().getTitle());
    }
    if (FunctionMagicWordType.IF_EXPR.equals(magicWordType)) {
      for (int param = 1; param < function.getParameterCount(); param++) {
        errorResult.addReplacement(function.getParameterValue(param));
      }
    }
    if ((analysis.isInTag(currentIndex, WikiTagType.GALLERY) == null) &&
        (analysis.isInTag(currentIndex, WikiTagType.INCLUDEONLY) == null) &&
        (analysis.isInTag(currentIndex, WikiTagType.REF) == null) &&
        (analysis.isInTag(currentIndex, WikiTagType.TIMELINE) == null)) {
      if ((!FunctionMagicWordType.INVOKE.equals(magicWordType)) &&
          (!FunctionMagicWordType.SUBST.equals(magicWordType)) &&
          (!FunctionMagicWordType.SAFE_SUBST.equals(magicWordType))) {
        errorResult.addReplacement(
            "{{subst:" +
                contents.substring(function.getBeginIndex() + 2, function.getEndIndex()));
      }
      errorResult.addReplacement(
          contents.substring(function.getBeginIndex(), function.getEndIndex()),
          GT._T("Expand"),
          true,
          false,
          false);
    }
    errors.add(errorResult);
    progress.currentIndex = function.getEndIndex();
    return true;
  }

  private boolean analyzeUnderscores(
      PageAnalysis analysis,
      Progress progress,
      Collection<CheckErrorResult> errors) {
    int currentIndex = progress.currentIndex;
    progress.currentIndex += 2;
    PageElementMagicWord magicWord = analysis.isInMagicWord(currentIndex);
    if (magicWord == null || magicWord.getBeginIndex() != currentIndex) {
      return false;
    }

    progress.currentIndex = magicWord.getEndIndex();
    MagicWordType magicWordType = magicWord.getMagicWord().getType();
    if (!SimpleMagicWordType.FORCE_TOC.equals(magicWordType) &&
        !SimpleMagicWordType.INDEX.equals(magicWordType) &&
        !SimpleMagicWordType.NO_INDEX.equals(magicWordType) &&
        !SimpleMagicWordType.NO_NEW_SECTION_LINK.equals(magicWordType)) {
      return false;
    }

    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, magicWord.getBeginIndex(), magicWord.getEndIndex());
    errorResult.addReplacement("");
    errors.add(errorResult);
    return true;
  }

  private static class Progress {
    public int currentIndex = 0;
  }
}
