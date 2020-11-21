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
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;


/**
 * Algorithm for analyzing error 58 of check wikipedia project.
 * Error 58: Headline ALL CAPS
 */
public class CheckErrorAlgorithm058 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm058() {
    super("Headline ALL CAPS");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Check every title
    List<PageElementTitle> titles = analysis.getTitles();
    boolean result = false;
    for (PageElementTitle title : titles) {
      String text = title.getTitle();
      if (text != null) {
        text = text.trim();

        // Count lower and upper case characters
        int upperCaseFound = 0;
        int lowerCaseFound = 0;
        int index = 0;
        while ((lowerCaseFound == 0) && (index < text.length())) {
          Character currentChar = text.charAt(index);
          if (Character.isUpperCase(currentChar)) {
            upperCaseFound++;
          }
          if (Character.isLowerCase(currentChar)) {
            lowerCaseFound++;
          }
          index++;
        }

        // Register error
        if ((lowerCaseFound == 0) && (upperCaseFound >= 10)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              title.getBeginIndex(), title.getEndIndex());
          StringBuilder sb = new StringBuilder(text);
          for (int i = 1; i < sb.length(); i++) {
            sb.setCharAt(i, Character.toLowerCase(sb.charAt(i)));
          }
          errorResult.addReplacement(TitleBuilder
              .from(title.getLevel(), sb.toString())
              .withAfter(title.getAfterTitle()).toString());
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
