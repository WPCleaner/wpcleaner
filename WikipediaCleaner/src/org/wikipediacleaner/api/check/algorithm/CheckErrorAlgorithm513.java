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
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 511 of check wikipedia project.
 * Error 513: Internal link inside external link
 */
public class CheckErrorAlgorithm513 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm513() {
    super("Internal link inside external link");
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
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      if ((link.hasSquare()) &&
          (link.getText() != null) &&
          (!link.hasSecondSquare())) {
        PageElementInternalLink internalLink = analysis.isInInternalLink(link.getEndIndex());
        if ((internalLink != null) &&
            (internalLink.getBeginIndex() == link.getEndIndex())) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), internalLink.getEndIndex());
          errorResult.addReplacement(
              contents.substring(link.getBeginIndex(), internalLink.getBeginIndex()) +
              internalLink.getDisplayedTextNotTrimmed());
          errorResult.addPossibleAction(new SimpleAction(
              GT._T("External Viewer"),
              new ActionExternalViewer(link.getLink())));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
