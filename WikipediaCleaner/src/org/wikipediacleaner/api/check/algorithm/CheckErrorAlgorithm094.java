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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 94 of check wikipedia project.
 * Error 94: Reference tags with no correct match
 */
public class CheckErrorAlgorithm094 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm094() {
    super("Reference tags with no correct match");
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

    // Analyze all tags
    boolean result = false;
    List<PageElementTag> refTags = analysis.getTags(WikiTagType.REF);
    if (refTags == null) {
      return result;
    }
    String contents = analysis.getContents();
    for (PageElementTag tag : refTags) {
      boolean shouldCheck = true;
      if (analysis.getSurroundingTag(WikiTagType.NOWIKI, tag.getBeginIndex()) != null) {
        shouldCheck = false;
      }

      if (shouldCheck && !tag.isFullTag() && !tag.isComplete()) {
        if (errors == null) {
          return true;
        }
        result = true;
        int beginIndex = tag.getBeginIndex();
        int endIndex = tag.getEndIndex();
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        errors.add(errorResult);
      } else if (tag.isEndTag()) {
        int beginIndex = tag.getBeginIndex();
        int endIndex = tag.getEndIndex();
        if ((contents.charAt(endIndex - 1) == '>') && (contents.charAt(endIndex - 2) == ' ')) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex, ErrorLevel.WARNING);
          errorResult.addReplacement(WikiTagType.REF.getCloseTag(), true);
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
