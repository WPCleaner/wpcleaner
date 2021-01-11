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
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 77 of check wikipedia project.
 * Error 77: Image description with partial &lt;small&gt;
 */
public class CheckErrorAlgorithm077 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm077() {
    super("Image description with partial <small>");
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


    // Analyzing all images
    boolean result = false;
    List<PageElementImage> images = analysis.getImages();
    for (PageElementImage image : images) {
      String description = image.getDescription();
      if (description != null) {
        description = description.trim();
        PageAnalysis descAnalysis = analysis.getPage().getAnalysis(description, false);
        List<PageElementTag> smallTags = descAnalysis.getTags(HtmlTagType.SMALL);
        if ((smallTags != null) && (!smallTags.isEmpty())) {
          int lastTest = 0;
          int currentDepth = 0;
          boolean onlySmall = true;
          for (PageElementTag smallTag : smallTags) {
            if ((currentDepth == 0) && (smallTag.getBeginIndex() > lastTest)) {
              onlySmall = false;
            }
            lastTest = smallTag.getEndIndex();
            if (!smallTag.isFullTag()) {
              if (smallTag.isEndTag()) {
                currentDepth = Math.max(0, currentDepth - 1);
              } else {
                currentDepth++;
              }
            }
          }
          if (!onlySmall) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, image.getBeginIndex(), image.getEndIndex());
            StringBuilder replacement = new StringBuilder();
            int lastIndex = 0;
            for (PageElementTag smallTag : smallTags) {
              if (smallTag.getBeginIndex() > lastIndex) {
                replacement.append(description.substring(lastIndex, smallTag.getBeginIndex()));
              }
              lastIndex = smallTag.getEndIndex();
            }
            if (lastIndex < description.length()) {
              replacement.append(description.substring(lastIndex));
            }
            errorResult.addReplacement(
                image.getDescriptionReplacement(replacement.toString()),
                GT._T("Remove {0} tags", HtmlTagType.SMALL.getOpenTag()));
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
