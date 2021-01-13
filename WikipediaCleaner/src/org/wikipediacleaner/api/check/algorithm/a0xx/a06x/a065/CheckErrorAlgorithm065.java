/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a065;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;


/**
 * Algorithm for analyzing error 65 of check wikipedia project.
 * Error 65: Image description with break
 */
public class CheckErrorAlgorithm065 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm065() {
    super("Image description with break");
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

    // Check that there are <br> tags in the text
    List<PageElementTag> brTags = analysis.getTags(HtmlTagType.BR);
    if ((brTags == null) || (brTags.isEmpty())) {
      return false;
    }

    // Check every image
    Collection<PageElementImage> images = analysis.getImages();
    if ((images == null) || (images.isEmpty())) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementImage image : images) {
      Parameter param = image.getDescriptionParameter();
      if (param != null) {

        // Check if error is present
        boolean breakFound = false;
        boolean tagAfter = false;
        boolean shouldStop = false;
        int currentIndex = image.getBeginIndex() + param.getEndOffset() - 1;
        int beginIndex = image.getBeginIndex() + param.getBeginOffset();
        int beginError = currentIndex + 1;
        int endError = currentIndex + 1;
        while ((currentIndex > beginIndex) && (!shouldStop)) {
          shouldStop = true;
          while ((currentIndex > beginIndex) &&
                 (Character.isWhitespace(contents.charAt(currentIndex)))) {
            if (!breakFound) {
              endError = currentIndex;
            }
            currentIndex--;
          }
          if (contents.charAt(currentIndex) == '>') {
            PageElementTag tag = analysis.isInTag(currentIndex);
            if (tag != null) {
              if (HtmlTagType.BR.equals(tag.getType())) {
                breakFound = true;
                shouldStop = false;
                beginError = tag.getBeginIndex();
                currentIndex = beginError - 1;
              } else if (!breakFound) {
                /*if (WikiTagType.MATH.equals(tag.getType())) {
                  tagAfter = true;
                  shouldStop = false;
                  endError = tag.getCompleteBeginIndex();
                  currentIndex = endError - 1;
                }*/
              }
            }
          }
        }

        // Report error
        if (breakFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginError, endError,
              (tagAfter ? ErrorLevel.WARNING : ErrorLevel.ERROR));
          if (!tagAfter) {
            errorResult.addReplacement("");
          }
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
