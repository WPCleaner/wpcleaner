/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
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

    boolean result = false;
    int currentIndex = 0;
    String contents = pageAnalysis.getContents();
    String br = PageElementTag.TAG_HTML_BR;
    int maxSize = contents.length();
    while (currentIndex < maxSize) {
      int nextIndex = currentIndex + 1;
      boolean shouldCheck = true;

      // Check if we are in a comment
      if (shouldCheck) {
        PageElementComment comment = pageAnalysis.isInComment(currentIndex);
        if (comment != null) {
          shouldCheck = false;
          nextIndex = comment.getEndIndex();
        }
      }

      // Check if this is a br tag
      if ((shouldCheck) && (contents.charAt(currentIndex) == '<')) {
        int tmpIndex = currentIndex + 1;
        while ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        boolean incorrectChar = false;
        while ((tmpIndex < maxSize) &&
               (" \\.:?/\n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
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
          while ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == ' ')) {
            tmpIndex++;
          }
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '/')) {
            tmpIndex++;
          }
          while ((tmpIndex < maxSize) &&
                 (" \\.:?/\n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
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
                  pageAnalysis.getPage(), currentIndex, tmpIndex);
              errorResult.addReplacement(PageElementTag.createTag(PageElementTag.TAG_HTML_BR, false, false));
              errors.add(errorResult);
              nextIndex = tmpIndex;
            }
          }
        }
      }

      currentIndex = nextIndex;
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
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
