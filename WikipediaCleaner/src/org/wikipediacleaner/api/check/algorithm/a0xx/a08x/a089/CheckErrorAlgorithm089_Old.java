/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a089;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 89 of check wikipedia project.
 * Error 89: DEFAULTSORT with capitalization in the middle of the word
 */
public class CheckErrorAlgorithm089_Old extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Fix DEFAULTSORT"),
  };

  public CheckErrorAlgorithm089_Old() {
    super("DEFAULTSORT with capitalization in the middle of the word");
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

    // Check every DEFAULTSORT
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    boolean result = false;
    for (PageElementFunction defaultSort : defaultSorts) {

      // Check if an upper case character is in the middle of a word
      boolean firstLetter = true;
      StringBuilder newText = null;
      String text = (defaultSort.getParameterCount() > 0) ? defaultSort.getParameterValue(0) : "";
      for (int index = 0; index < text.length(); index++) {
        char currentChar = text.charAt(index);
        if (Character.isUpperCase(currentChar)) {
          if (!firstLetter) {
            if (newText == null) {
              newText = new StringBuilder(text.substring(0, index));
            }
            currentChar = Character.toLowerCase(currentChar);
          }
          firstLetter = false;
        } else if (Character.isLowerCase(currentChar)) {
          firstLetter = false;
        } else {
          firstLetter = true;
        }
        if (newText != null) {
          newText.append(currentChar);
        }
      }

      // Register error
      if (newText != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            defaultSort.getBeginIndex(), defaultSort.getEndIndex());
        errorResult.addReplacement(PageElementFunction.createFunction(
            defaultSort.getFunctionName(), newText.toString()));
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
