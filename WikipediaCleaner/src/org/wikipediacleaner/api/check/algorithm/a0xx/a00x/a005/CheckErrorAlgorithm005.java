/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a00x.a005;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 05 of check wikipedia project.
 * Error 05: Found a comment "&lt;!--" with no "--&gt;" end.
 */
public class CheckErrorAlgorithm005 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm005() {
    super("Found a comment \"" + ContentsComment.START + "\" with no \"" + ContentsComment.END + "\" end.");
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

    // Analyze contents from the end
    boolean result = false;
    String contents = analysis.getContents();
    boolean inComment = false;
    int possibleEndIndex = -1;
    int previousStartIndex = -1;
    int currentIndex = contents.length();
    while (currentIndex > 0) {
      currentIndex--;
      if (contents.startsWith(ContentsComment.END, currentIndex)) {
        inComment = true;
        possibleEndIndex = -1;
        previousStartIndex = -1;
        currentIndex -= 3;
      } else if (contents.startsWith("->", currentIndex)) {
        possibleEndIndex = currentIndex;
      } else if (contents.startsWith(ContentsComment.START, currentIndex)) {
        if (inComment) {
          inComment = false;
        } else {
          if (errors == null) {
            return true;
          }
          result = true;
          int nextIndex = currentIndex + 4;
          while ((nextIndex < contents.length()) &&
                 (contents.charAt(nextIndex) == ' ')) {
            nextIndex++;
          }
          CheckErrorResult errorResult = null;
          if (possibleEndIndex > 0) {
            errorResult = createCheckErrorResult(
                analysis, currentIndex, possibleEndIndex + 2);
            errorResult.addReplacement(
                contents.substring(currentIndex, possibleEndIndex) + ContentsComment.END,
                GT._T("Properly end the comment"));
          } else if (previousStartIndex > 0) {
            int tmpIndex = previousStartIndex;
            while ((tmpIndex > 0) &&
                   ((contents.charAt(tmpIndex - 1) == '\n') ||
                    (contents.charAt(tmpIndex - 1) == ' '))) {
              tmpIndex--;
            }
            if (tmpIndex < currentIndex + 5) {
              tmpIndex = currentIndex + 5;
            }
            int endIndex = previousStartIndex + 4;
            errorResult = createCheckErrorResult(
                analysis, currentIndex, endIndex);
            int lineEndIndex = ContentsUtil.getLineEndIndex(contents, currentIndex);
            if ((lineEndIndex < tmpIndex) && (lineEndIndex > currentIndex + 4)) {
              errorResult.addReplacement(
                  contents.substring(currentIndex, lineEndIndex) + ContentsComment.END + contents.substring(lineEndIndex, endIndex),
                  GT._T("End the comment at the end of the line"));
            }
            errorResult.addReplacement(
                contents.substring(currentIndex, tmpIndex) + ContentsComment.END + contents.substring(tmpIndex, endIndex),
                GT._T("End the comment just before the next one begins"));
            errorResult.addReplacement(
                contents.substring(currentIndex, previousStartIndex),
                GT._T("Merge comments"));
            errorResult.addReplacement(
                contents.substring(nextIndex, endIndex),
                GT._T("Uncomment"));
          } else {
            errorResult = createCheckErrorResult(
                analysis, currentIndex, nextIndex);
            errorResult.addReplacement("", GT._T("Uncomment"));
          }
          errors.add(errorResult);
        }
        previousStartIndex = currentIndex;
      }
    }

    return result;
  }
}
