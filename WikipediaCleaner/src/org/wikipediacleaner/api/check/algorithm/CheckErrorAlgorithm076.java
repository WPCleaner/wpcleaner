/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 76 of check wikipedia project.
 * Error 76: Link with no space
 */
public class CheckErrorAlgorithm076 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Replace all %20 by space character"),
  };

  public CheckErrorAlgorithm076() {
    super("Link with no space");
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

    // Analyze each internal link
    boolean result = false;
    for (PageElementInternalLink link : analysis.getInternalLinks()) {
      int spaceIndex = link.getFullLink().indexOf("%20");
      if (spaceIndex >= 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        errorResult.addReplacement(
            InternalLinkBuilder.from(link.getFullLink().replaceAll("\\%20", " "))
            .withText(link.getText()).toString(),
            GT._T("Replace %20 by space character"));
        errors.add(errorResult);
      }
    }

    // Analyze each external link
    String contents = analysis.getContents();
    for (PageElementExternalLink link : analysis.getExternalLinks()) {
      int beginIndex = link.getBeginIndex();
      int endIndex = link.getEndIndex();
      if (link.hasSquare() &&
          (beginIndex > 0) && (contents.charAt(beginIndex - 1) == '[') &&
          (endIndex < contents.length()) && (contents.charAt(endIndex) == ']')) {
        int spaceIndex = link.getLink().indexOf("%20");
        if (spaceIndex >= 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex - 1, endIndex + 1);
          errorResult.addReplacement(contents.substring(beginIndex, endIndex));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
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
