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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 44 of check wikipedia project.
 * Error 44: Headlines with bold
 */
public class CheckErrorAlgorithm044 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix all bold headlines"),
  };

  public CheckErrorAlgorithm044() {
    super("Headlines with bold");
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

    // Check every title
    List<PageElementTitle> titles = analysis.getTitles();
    boolean result = false;
    for (PageElementTitle title : titles) {
      String text = title.getTitle();
      if (text != null) {
        text = text.trim();

        // Check if the title is bold
        int index = 0;
        int countBold = 0;
        while (index < text.length()) {
          if (text.startsWith("'''", index)) {
            index += 3;
            countBold++;
          } else {
            index++;
          }
        }

        // Register error
        if (countBold > 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          text = text.replaceAll("'''", "");
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              title.getBeginIndex(), title.getEndIndex());
          errorResult.addReplacement(
              PageElementTitle.createTitle(
                  title.getLevel(), text, title.getAfterTitle()),
              false,
              (countBold == 2) && text.startsWith("'''") && text.endsWith("'''"));
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
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
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
    return fixUsingAutomaticBotReplacement(analysis);
  }
}
