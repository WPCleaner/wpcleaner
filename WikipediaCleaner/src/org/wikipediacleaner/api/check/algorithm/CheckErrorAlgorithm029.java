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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 29 of check wikipedia project.
 * Error 29: Gallery not correct end
 */
public class CheckErrorAlgorithm029 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm029() {
    super("Gallery not correct end");
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

    // Check every <gallery> tag
    List<PageElementTag> galleryTags = analysis.getTags(WikiTagType.GALLERY);
    String contents = analysis.getContents();
    boolean result = false;
    int index = 0;
    while (index < galleryTags.size()) {

      // Check for error
      PageElementTag galleryTag = galleryTags.get(index);
      boolean found = false;
      if (galleryTag.isFullTag() || !galleryTag.isComplete()) {
        found = true;
      }
      int beginIndex = galleryTag.getBeginIndex();
      if (found) {
        if (analysis.getSurroundingTag(WikiTagType.NOWIKI, beginIndex) != null) {
          found = false;
        }
      }

      if (found) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check if an other <gallery> tag is just after and can be used
        PageElementTag nextTag = null;
        if (!galleryTag.isEndTag() && (index + 1 < galleryTags.size())) {
          nextTag = galleryTags.get(index + 1);
          int currentIndex = galleryTag.getEndIndex();
          while ((nextTag != null) && (currentIndex < nextTag.getBeginIndex())) {
            char currentChar = contents.charAt(currentIndex);
            if ((currentChar != ' ') && (currentChar != '\n')) {
              nextTag = null;
            }
            currentIndex++;
          }
          if (nextTag != null) {
            if ((galleryTag.getParametersCount() > 0) &&
                (nextTag.getParametersCount() > 0)) {
              nextTag = null;
            }
          }
        }

        // Report error
        int endIndex = (nextTag != null) ? nextTag.getEndIndex() : galleryTag.getEndIndex();
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        if (nextTag == null) {
          errorResult.addReplacement("");
          index++;
        } else if (nextTag.getParametersCount() == 0) {
          errorResult.addReplacement(contents.substring(
              galleryTag.getBeginIndex(), galleryTag.getEndIndex()));
          errorResult.addReplacement(contents.substring(
              nextTag.getBeginIndex(), nextTag.getEndIndex()));
          index += 2;
        } else {
          errorResult.addReplacement(contents.substring(
              nextTag.getBeginIndex(), nextTag.getEndIndex()));
          errorResult.addReplacement(contents.substring(
              galleryTag.getBeginIndex(), galleryTag.getEndIndex()));
          index += 2;
        }
        errors.add(errorResult);
      } else {
        index++;
      }
    }

    return result;
  }
}
