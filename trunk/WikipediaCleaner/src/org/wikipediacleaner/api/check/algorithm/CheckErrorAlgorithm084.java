/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 84 of check wikipedia project.
 * Error 84: Section without content
 */
public class CheckErrorAlgorithm084 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm084() {
    super("Section without content");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieving titles
    boolean result = false;
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    if (titles == null) {
      return false;
    }

    // Retrieve texts that can be added to sections without content
    String allTexts = getSpecificProperty("texts", true, true, false);
    List<String> texts = null;
    if (allTexts != null) {
      texts = WPCConfiguration.convertPropertyToStringList(allTexts);
    }

    // Analyzing titles
    String contents = pageAnalysis.getContents();
    for (int i = 0; i < titles.size(); i++) {
      PageElementTitle title = titles.get(i);
      PageElementTitle nextTitle = (i + 1 < titles.size()) ? titles.get(i + 1) : null;
      if ((nextTitle == null) ||
          (nextTitle.getLevel() <= title.getLevel())) {
        boolean textFound = false;
        int lastPos = (nextTitle != null) ? nextTitle.getBeginIndex() : contents.length(); 
        int pos = title.getEndIndex();
        PageElementComment commentFound = null;
        while (!textFound && (pos < lastPos)) {
          char currentChar = contents.charAt(pos);
          if (Character.isWhitespace(currentChar)) {
            pos++;
          } else if (currentChar == '<') {
            PageElementComment comment = pageAnalysis.isInComment(pos);
            if (comment != null) {
              pos = comment.getEndIndex();
              if (commentFound == null) {
                commentFound = comment;
              }
            } else {
              textFound = true;
            }
          } else {
            textFound = true;
          }
        }
        if (!textFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          if (commentFound != null) {
            lastPos = commentFound.getBeginIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), title.getBeginIndex(), lastPos);
          if (texts != null) {
            for (String text : texts) {
              String replacement =
                  contents.substring(title.getBeginIndex(), title.getEndIndex()) + "\n" +
                  text + "\n\n";
              errorResult.addReplacement(replacement, GT._("Add {0}", text));
            }
          }
          errorResult.addReplacement("", GT._("Delete section"));
          if ((nextTitle != null) && (nextTitle.getLevel() == title.getLevel())) {
            errorResult.addEditTocAction();
          }
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("texts", GT._("A list of texts that can be added to sections without content"));
    return parameters;
  }
}
