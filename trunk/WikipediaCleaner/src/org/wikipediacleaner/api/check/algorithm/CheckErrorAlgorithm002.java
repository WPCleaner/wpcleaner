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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 2 of check wikipedia project.
 * Error 2: Article with false &lt;br&gt;
 */
public class CheckErrorAlgorithm002 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix all <br> tags"),
  };

  public CheckErrorAlgorithm002() {
    super("Article with false <br>");
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

    // Check for incorrect br tags
    boolean result = false;
    int currentIndex = 0;
    String contents = analysis.getContents();
    String br = PageElementTag.TAG_HTML_BR;
    int maxSize = contents.length();
    while (currentIndex < maxSize) {
      int nextIndex = currentIndex + 1;
      boolean shouldCheck = true;

      // Check if we are in a comment
      if (shouldCheck) {
        PageElementComment comment = analysis.isInComment(currentIndex);
        if (comment != null) {
          shouldCheck = false;
          nextIndex = comment.getEndIndex();
        }
      }

      // Check if this is a br tag
      if ((shouldCheck) && (contents.charAt(currentIndex) == '<')) {
        int tmpIndex = getFirstIndexAfterSpace(contents, currentIndex + 1);
        boolean incorrectChar = false;
        while ((tmpIndex < maxSize) &&
               (" \\.,:?/\n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
          tmpIndex++;
          incorrectChar = true;
        }
        boolean brTag = true;
        for (int i = 0; i < br.length(); i++) {
          if ((tmpIndex >= maxSize) ||
              (Character.toUpperCase(contents.charAt(tmpIndex)) != Character.toUpperCase(br.charAt(i)))) {
            brTag = false;
          }
          tmpIndex++;
        }
        if ((tmpIndex < maxSize) && (brTag)) {
          tmpIndex = getFirstIndexAfterSpace(contents, tmpIndex);
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '/')) {
            tmpIndex++;
          }
          tmpIndex = getFirstIndexAfterSpace(contents, tmpIndex);
          while ((tmpIndex < maxSize) &&
                 (" \\.,:?/\n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
            incorrectChar = true;
          }
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '>')) {
            if (incorrectChar) {
              if (errors == null) {
                return true;
              }
              result = true;
              tmpIndex++;
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, currentIndex, tmpIndex);
              errorResult.addReplacement(
                  PageElementTag.createTag(PageElementTag.TAG_HTML_BR, false, false),
                  true);
              errors.add(errorResult);
              nextIndex = tmpIndex;
            }
          }
        }
      }

      currentIndex = nextIndex;
    }

    // Check for br tags with extra characters
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_HTML_BR);
    for (PageElementTag tag : tags) {

      // Check for "clear" attribute
      Parameter clearParameter = tag.getParameter("clear");

      // Check for extra characters before the br tag
      boolean extra = false;
      int beginIndex = tag.getBeginIndex();
      while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '<')) {
        beginIndex--;
        extra = true;
      }

      // Check for extra characters after the br tag
      int endIndex = tag.getEndIndex();
      while ((endIndex < contents.length()) && (contents.charAt(endIndex) == '>')) {
        endIndex++;
        extra  = true;
      }

      if (extra || (clearParameter != null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex, ErrorLevel.WARNING);
        if (clearParameter != null) {
          String clearValue = clearParameter.getTrimmedValue();
          String clearReplacementName = null;
          if ("all".equalsIgnoreCase(clearValue) || "both".equalsIgnoreCase(clearValue)) {
            clearReplacementName = "clear_all";
          } else if ("left".equalsIgnoreCase(clearValue)) {
            clearReplacementName = "clear_left";
          } else if ("right".equalsIgnoreCase(clearValue)) {
            clearReplacementName = "clear_right";
          }
          String clearReplacement = null;
          if (clearReplacementName != null) {
            clearReplacement = getSpecificProperty(clearReplacementName, true, true, false);
          }
          if (clearReplacement != null) {
            errorResult.addReplacement(clearReplacement, !clearReplacement.isEmpty());
          }
        }
        if (extra) {
          errorResult.addReplacement(
              PageElementTag.createTag(PageElementTag.TAG_HTML_BR, false, false),
              false);
        }
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
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("clear_all", GT._("A replacement for {0}", "&lt;br clear=\"all\"/&gt;"));
    parameters.put("clear_left", GT._("A replacement for {0}", "&lt;br clear=\"left\"/&gt;"));
    parameters.put("clear_right", GT._("A replacement for {0}", "&lt;br clear=\"right\"/&gt;"));
    return parameters;
  }
}
