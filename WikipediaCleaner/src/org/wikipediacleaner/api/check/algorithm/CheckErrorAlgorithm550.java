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
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 550 of check wikipedia project.
 * Error 550: Link without text.
 */
public class CheckErrorAlgorithm550 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm550() {
    super("Link without text");
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

    // Check each link
    boolean result = false;
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    for (PageElementInternalLink link : links) {

      // Analyze link
      String target = link.getFullLink();
      String text = link.getDisplayedTextNotTrimmed();
      boolean shouldReport = false;
      boolean automatic = false; // TODO: test with true
      if ((target != null) && (!target.isEmpty())) {
        if ((text != null) && !text.isEmpty()) {
          shouldReport = true;
          int index = 0;
          while (shouldReport && (index < text.length())) {
            char currentChar = text.charAt(index);
            if (currentChar == '<') {
              PageElementTag tag = PageElementTag.analyzeBlock(text, index);
              if ((tag != null) &&
                  PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName())) {
                index = tag.getEndIndex();
              } else {
                shouldReport = false;
              }
            } else if (CharacterUtils.isWhitespace(currentChar)) {
              index++;
              automatic = false;
            } else {
              shouldReport = false;
            }
          }
        }
      }

      // Report error
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        errorResult.addReplacement("", automatic);
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
