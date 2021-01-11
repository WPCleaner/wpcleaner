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
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 505 of check wikipedia project.
 * Error 506: Reference with a numeric name
 */
public class CheckErrorAlgorithm506 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm506() {
    super("Reference with a numeric name");
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

    // Check every ref tag
    Collection<PageElementTag> refTags = analysis.getCompleteTags(WikiTagType.REF);
    if ((refTags == null) || (refTags.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTag refTag : refTags) {
      PageElementTag.Parameter paramName = refTag.getParameter("name");
      if ((paramName != null) &&
          (paramName.getTrimmedValue() != null) &&
          (paramName.getTrimmedValue().length() > 0)) {
        String name = paramName.getTrimmedValue();
        boolean hasNonNumericCharacter = false;
        int index = name.length();
        while ((!hasNonNumericCharacter) && (index > 0)) {
          index--;
          if (!Character.isDigit(name.charAt(index))) {
            hasNonNumericCharacter = true;
          }
        }
        if (!hasNonNumericCharacter) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult error = createCheckErrorResult(
              analysis,
              refTag.getBeginIndex(), refTag.getEndIndex());
          errors.add(error);
        }
      }
    }

    return result;
  }
}
