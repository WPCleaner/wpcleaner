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
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 103 of check wikipedia project.
 * Error 103: Unnecessary pipe template
 */
public class CheckErrorAlgorithm103 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm103() {
    super("Unnecessary pipe template");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check every pipe template
    List<PageElementTemplate> pipeTemplates = analysis.getTemplates("!");
    if ((pipeTemplates == null) || (pipeTemplates.isEmpty())) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTemplate pipeTemplate : pipeTemplates) {
      int beginIndex = pipeTemplate.getBeginIndex();
      int endIndex = pipeTemplate.getEndIndex();
      int startBefore = contents.lastIndexOf("[[", beginIndex);
      int endBefore = contents.lastIndexOf("]]", beginIndex);
      int startAfter = contents.indexOf("[[", endIndex);
      int endAfter = contents.indexOf("]]", endIndex);

      // Is it between [[ and ]] ?
      boolean shouldKeep = false;
      if ((startBefore >= 0) && (endAfter >= 0)) {
        if (endBefore < startBefore) {
          if ((startAfter < 0) || (startAfter > endAfter)) {
            shouldKeep = true;
          }
        }
      }

      // Are the [[ and ]] for an image ?
      if (shouldKeep) {
        PageElementImage image = analysis.isInImage(beginIndex);
        if (image != null) {
          if (startBefore == image.getBeginIndex()) {
            shouldKeep = false;
          }
          if (endAfter == image.getEndIndex() - 2) {
            shouldKeep = false;
          }
        }
      }

      // Report error
      if (shouldKeep) {
        ErrorLevel level = ErrorLevel.ERROR;
        int pipeBefore = contents.lastIndexOf("|", beginIndex);
        if (pipeBefore > startBefore) {
          level = ErrorLevel.WARNING;
        }
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex, level);
        if (level == ErrorLevel.ERROR) {
          errorResult.addReplacement("|");
        }
        errors.add(errorResult);
      }
    }

    return result;
  }
}
