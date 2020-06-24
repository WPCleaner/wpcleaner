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
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 538 of check wikipedia project.
 * Error 538: Whitespace characters after heading
 */
public class CheckErrorAlgorithm538 extends CheckErrorAlgorithmBase {

  /** Possible global fixes */
  private final static String[] globalFixes = new String[] {
    GT._T("Remove whitespace characters"),
  };

  /** Whitespace characters to check */
  private final static String WHITESPACE = " \t";

  public CheckErrorAlgorithm538() {
    super("Whitespace characters after heading");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each title
    List<PageElementTitle> titles = analysis.getTitles();
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTitle title : titles) {

      // Check if error is present
      int beginIndex = title.getEndIndex();
      if ((beginIndex < contents.length()) &&
          (WHITESPACE.indexOf(contents.charAt(beginIndex)) >= 0)) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Determine area
        int endIndex = beginIndex + 1;
        while ((endIndex < contents.length()) &&
               (WHITESPACE.indexOf(contents.charAt(endIndex)) >= 0)) {
          endIndex++;
        }

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        errorResult.addReplacement("", true);
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
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
    return fixUsingAutomaticReplacement(analysis);
  }
}