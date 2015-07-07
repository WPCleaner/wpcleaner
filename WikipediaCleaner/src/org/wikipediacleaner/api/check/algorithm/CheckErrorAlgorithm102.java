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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementPMID;


/**
 * Algorithm for analyzing error 102 of check wikipedia project.
 * Error 102: PMID wrong syntax
 */
public class CheckErrorAlgorithm102 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm102() {
    super("PMID wrong syntax");
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

    // Analyze each PMID
    boolean result = false;
    List<PageElementPMID> pmids = analysis.getPMIDs();
    for (PageElementPMID pmid : pmids) {
      if (!pmid.isCorrect() && pmid.isValid()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(analysis, pmid, false);
        errors.add(errorResult);
        List<String> replacements = pmid.getCorrectPMID();
        if (replacements != null) {
          for (String replacement : replacements) {
            if (!replacement.equals(analysis.getContents().substring(pmid.getBeginIndex(), pmid.getEndIndex()))) {
              errorResult.addReplacement(replacement);
            }
          }
        }
      }
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param pmid PMID.
   * @param checkForComment True to check for a comment after the PMID.
   * @return Error result.
   */
  protected CheckErrorResult createCheckErrorResult(
      PageAnalysis analysis, PageElementPMID pmid,
      boolean checkForComment) {
    ErrorLevel level = (pmid.isValid() && !pmid.helpRequested()) ?
        ErrorLevel.ERROR : ErrorLevel.WARNING;
    if (checkForComment) {
      String contents = analysis.getContents();
      int index = pmid.getEndIndex();
      while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
        index++;
      }
      if ((index < contents.length()) && (contents.charAt(index) == '<')) {
        PageElementComment comment = analysis.isInComment(index);
        if (comment != null) {
          level = ErrorLevel.WARNING;
        }
      }
    }
    CheckErrorResult result = createCheckErrorResult(
        analysis, pmid.getBeginIndex(), pmid.getEndIndex(), level);
    return result;
  }
}
