/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a082;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 82 of check wikipedia project.
 * Error 82: Link to other wiki project
 */
public class CheckErrorAlgorithm082 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm082() {
    super("Link to other wikiproject");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = -1;
    String contents = analysis.getContents();
    while (startIndex < contents.length()) {
      PageElementInterwikiLink link = analysis.getNextInterwikiLink(startIndex);
      if ((link != null) &&
          (link.getInterwiki() != null) &&
          (link.getInterwiki().getLanguage() == null)) {
        startIndex = link.getEndIndex();
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        if (link.getText() != null) {
          errorResult.addReplacement(link.getText());
        } else {
          errorResult.addReplacement(link.getLink());
        }
        errors.add(errorResult);
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}
