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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 59 of check wikipedia project.
 * Error 59: Template value end with break
 */
public class CheckErrorAlgorithm059 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Delete all"),
  };

  public CheckErrorAlgorithm059() {
    super("Template value end with break");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check that there are <br> tags in the text
    List<PageElementTag> brTags = analysis.getTags(PageElementTag.TAG_HTML_BR);
    if ((brTags == null) || (brTags.isEmpty())) {
      return false;
    }

    // Analyzing each template
    boolean result = false;
    for (PageElementTemplate template : analysis.getTemplates()) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {

        // Search for <br> at the end of the parameter
        String paramValue = template.getParameterValue(paramNum);
        boolean breakFound = false;
        boolean tagAfter = false;
        int currentValuePos = paramValue.length() - 1;
        int beginError = -1;
        int endError = -1;
        boolean shouldStop = false;
        while (!shouldStop) {
          shouldStop = true;
          while ((currentValuePos > 0) &&
                 (Character.isWhitespace(paramValue.charAt(currentValuePos)))) {
            currentValuePos--;
          }
          if ((currentValuePos > 0) &&
              (paramValue.charAt(currentValuePos) == '>')) {
            PageElementTag tag = analysis.isInTag(
                template.getBeginIndex() +
                template.getParameterValueOffset(paramNum) +
                currentValuePos);
            if (tag != null) {
              String name = tag.getNormalizedName();
              if (PageElementTag.TAG_HTML_BR.equals(name)) {
                breakFound = true;
                shouldStop = false;
                beginError = tag.getBeginIndex();
                if (endError < 0) {
                  endError = tag.getEndIndex();
                }
                currentValuePos -= tag.getEndIndex() - tag.getBeginIndex();
              } else if (!breakFound) {
                /*if (PageElementTag.TAG_WIKI_MATH.equals(name)) {
                  tagAfter = true;
                  shouldStop = false;
                  endError = tag.getCompleteBeginIndex();
                  currentValuePos -= tag.getEndIndex() - tag.getCompleteBeginIndex();
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

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  // No Bot fixing
  // @Override
  //public String botFix(PageAnalysis analysis) {
  //  return fix(globalFixes[0], analysis, null);
  //}

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
