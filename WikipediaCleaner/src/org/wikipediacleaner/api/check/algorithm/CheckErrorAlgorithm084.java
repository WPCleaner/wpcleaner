/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsComment;
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

    // Retrieving titles
    boolean result = false;
    List<PageElementTitle> titles = analysis.getTitles();
    if (titles == null) {
      return false;
    }

    // Analyzing titles
    String contents = analysis.getContents();
    for (int i = 0; i < titles.size(); i++) {
      PageElementTitle title = titles.get(i);
      PageElementTitle nextTitle = (i + 1 < titles.size()) ? titles.get(i + 1) : null;
      if ((nextTitle == null) ||
          (nextTitle.getLevel() <= title.getLevel())) {
        boolean textFound = false;
        int lastPos = (nextTitle != null) ? nextTitle.getBeginIndex() : contents.length(); 
        int pos = title.getEndIndex();
        ContentsComment commentFound = null;
        while (!textFound && (pos < lastPos)) {
          char currentChar = contents.charAt(pos);
          if (Character.isWhitespace(currentChar)) {
            pos++;
          } else if (currentChar == '<') {
            ContentsComment comment = analysis.comments().getAt(pos);
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
        if (!textFound && (title.getFirstLevel() <= maxLevel)) {
          if (errors == null) {
            return true;
          }
          result = true;
          if (commentFound != null) {
            lastPos = commentFound.getBeginIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, title.getBeginIndex(), lastPos);
          if (texts != null) {
            for (String text : texts) {
              String replacement =
                  contents.substring(title.getBeginIndex(), title.getEndIndex()) + "\n" +
                  text + "\n\n";
              errorResult.addReplacement(replacement, GT._T("Add {0}", text));
            }
          }
          errorResult.addReplacement("", GT._T("Delete section"));
          if ((nextTitle != null) && (nextTitle.getLevel() == title.getLevel())) {
            errorResult.addEditTocAction(title);
          }
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Texts that can be added */
  private static final String PARAMETER_TEXTS = "texts";

  /** Restrict checks to some levels for the titles */
  private static final String PARAMETER_LEVEL = "level";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEXTS, true, true, false);
    texts.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        texts.addAll(tmpList);
      }
    }

    maxLevel = 2;
    tmp = getSpecificProperty(PARAMETER_LEVEL, true, true, false);
    if (tmp != null) {
      try {
        maxLevel = Integer.parseInt(tmp);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
    }
  }

  /** Texts that can be added */
  private final List<String> texts = new ArrayList<>();

  /** Restrict checks to some levels for the titles */
  private int maxLevel = 2;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEXTS,
        GT._T("A list of texts that can be added to sections without content"),
        new AlgorithmParameterElement(
            "text",
            GT._T("A text that can be added to sections without content"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_LEVEL,
        GT._T("Restrict verification to titles with a higher level"),
        new AlgorithmParameterElement(
            "level",
            GT._T("Maximum level of title to check"))));
  }
}
