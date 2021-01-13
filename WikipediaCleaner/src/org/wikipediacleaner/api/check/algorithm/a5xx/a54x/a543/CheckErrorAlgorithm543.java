/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a543;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 543 of check wikipedia project.
 * Error 543: Pipe in external link.
 */
public class CheckErrorAlgorithm543 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm543() {
    super("Pipe in external link");
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

    // Analyze each external link
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {

      // Check if the link target contains a pipe
      boolean hasPipe = false;
      String target = link.getLink();
      int pipeIndex = target.indexOf('|');
      if (pipeIndex > 0) {
        pipeIndex += link.getLinkBeginIndex();
      }

      // Check if the link target has a pipe just after
      if (!hasPipe && link.hasSquare()) {
        int index = link.getLinkEndIndex();
        if ((index < contents.length()) && (contents.charAt(index) == '|')) {
          pipeIndex = index;
        }
      }

      // Final verification
      int newStartIndex = pipeIndex;
      boolean automatic = false;
      if ((pipeIndex > 0) && (pipeIndex + 1 < link.getEndIndex())) {
        int tmpIndex = pipeIndex + 1;
        if (contents.charAt(tmpIndex) == ' ') {
          hasPipe = true;
          newStartIndex = tmpIndex + 1;
          automatic = true;
        }
        if ("{[".indexOf(contents.charAt(tmpIndex)) >= 0) {
          hasPipe = true;
          newStartIndex = tmpIndex;
        } else {
          while ((tmpIndex < link.getEndIndex()) &&
                 (Character.isLetterOrDigit(contents.charAt(tmpIndex)))) {
            tmpIndex++;
          }
          String prefix = contents.substring(pipeIndex + 1, tmpIndex);
          while ((tmpIndex < link.getEndIndex()) && (contents.charAt(tmpIndex) == ' ')) {
            tmpIndex++;
          }
          if ((prefix.length() > 0) &&
              (contents.startsWith(prefix, tmpIndex))) {
            hasPipe = true;
            newStartIndex = tmpIndex;
          }
        }
      }

      // Report error
      if (hasPipe) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check for automatic modifications
        if (!link.hasSquare() || !link.hasSecondSquare()) {
          automatic = false;
        }

        // Report error
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        CheckErrorResult error = createCheckErrorResult(analysis, beginIndex, endIndex);
        error.addReplacement(
            contents.substring(beginIndex, pipeIndex) + ' ' + contents.substring(newStartIndex, endIndex),
            automatic);
        errors.add(error);
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
        analysis.getPage().isInUserNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
