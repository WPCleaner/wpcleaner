/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 551 of check wikipedia project.
 * Error 551: Empty line.
 */
public class CheckErrorAlgorithm551 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm551() {
    super("Empty line");
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

    // Check each line
    String contents = analysis.getContents();
    int currentIndex = 0;
    boolean result = false;
    while (currentIndex < contents.length()) {
      int beginLine = currentIndex;
      boolean emptyLine = true;
      boolean something = false;
      int countItalic = 0;
      int countBold = 0;
      while (emptyLine && (currentIndex < contents.length())) {
        char currentChar = contents.charAt(currentIndex);
        if (currentChar == '<') {
          PageElementTag tag = analysis.isInTag(currentIndex);
          if ((tag != null) &&
              (tag.getBeginIndex() == currentIndex) &&
              (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName()))) {
            currentIndex = tag.getEndIndex();
            something = true;
          } else {
            emptyLine = false;
          }
        } else if (currentChar == '\'') {
          something = true;
          int count = 1;
          while ((currentIndex + count < contents.length()) &&
                 (contents.charAt(currentIndex + count) == '\'')) {
            count++;
          }
          switch (count) {
          case 2:
            countItalic++;
            break;
          case 3:
            countBold++;
            break;
          case 5:
            countItalic++;
            countBold++;
            break;
          default:
            emptyLine = false;
          }
          currentIndex += count;
        } else if (currentChar == ' ') {
          currentIndex++;
        } else if (currentChar == '\n') {
          break;
        } else {
          emptyLine = false;
        }
      }

      // Go to the end of the line
      while ((currentIndex < contents.length()) &&
          (contents.charAt(currentIndex) != '\n')) {
        currentIndex++;
      }
      if (currentIndex < contents.length()) {
        currentIndex++;
      }

      // Report error if needed
      if (emptyLine && something) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Decide if it can be automatic
        boolean automatic = false; // TODO: Test with true
        if ((countItalic % 2 != 0) || (countBold % 2 != 0)) {
          automatic = false;
        }

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginLine, currentIndex);
        errorResult.addReplacement("", automatic);
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of links to be ignored */
  private static final String PARAMETER_IGNORE_LINKS = "ignore_links";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_LINKS, true, true, true);
    ignoreLinks.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        ignoreLinks.addAll(tmpList);
      }
    }
  }

  /** Links to ignore */
  private final Set<String> ignoreLinks = new HashSet<>();

  /**
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(PARAMETER_IGNORE_LINKS, GT._T("Links to ignore"));
    return parameters;
  }
}
