/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 522 of check wikipedia project.
 * Error 522: Empty title
 */
public class CheckErrorAlgorithm522 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm522() {
    super("Empty title");
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
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTitle title : analysis.getTitles()) {

      // Check if there's something in the title
      boolean textFound = false;
      int currentIndex = title.getBeginIndex();
      int lastIndex = title.getEndIndex();
      while ((currentIndex < lastIndex) &&
             (contents.charAt(currentIndex) == '=')) {
        currentIndex++;
      }
      while (!textFound &&
             (currentIndex < lastIndex) &&
             (contents.charAt(currentIndex) != '=')) {
        currentIndex = ContentsUtil.moveIndexAfterWhitespace(contents, currentIndex);
        if (currentIndex < lastIndex) {
          ContentsComment comment = null;
          PageElementTag tag = null;
          char currentChar = contents.charAt(currentIndex);
          if (currentChar == '<') {
            comment = analysis.comments().getAt(currentIndex);
            tag = analysis.isInTag(currentIndex, WikiTagType.NOWIKI);
          }
          if (comment != null) {
            currentIndex = comment.getEndIndex();
          } else if (tag != null) {
            currentIndex = tag.getCompleteEndIndex();
          } else if (currentChar != '=') {
            if (!Character.isWhitespace(currentChar)) {
              textFound = true;
            }
            currentIndex++;
          }
        }
      }

      // Report error
      if (!textFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        if ((lastIndex < contents.length()) &&
            (contents.charAt(lastIndex) == '\n')) {
          lastIndex++;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, title.getBeginIndex(), lastIndex);
        errorResult.addReplacement("");
        errors.add(errorResult);
      }
    }

    return result;
  }
}
