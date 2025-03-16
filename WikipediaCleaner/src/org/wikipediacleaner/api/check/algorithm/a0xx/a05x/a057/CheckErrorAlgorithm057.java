/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a05x.a057;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 57 of check wikipedia project.
 * Error 57: Headlines end with colon
 */
public class CheckErrorAlgorithm057 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Fix all titles"),
  };

  public CheckErrorAlgorithm057() {
    super("Headlines end with colon");
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
      result |= analyzeTitle(analysis, title, errors);
    }

    return result;
  }

  /**
   * Analyze a title to check if errors are present.
   *
   * @param analysis Page analysis.
   * @param title Title to analyze.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTitle(
          PageAnalysis analysis,
          PageElementTitle title,
          Collection<CheckErrorResult> errors) {
    String text = title.getTitle();
    if (text == null) {
      return false;
    }

    text = text.trim();
    if (!text.endsWith(":")) {
      return false;
    }

    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            title.getBeginIndex(), title.getEndIndex());
    final String newTitle = text.substring(0, text.length() - 1).trim();
    final String newFormattedTitle = TitleBuilder
            .from(title.getLevel(), newTitle)
            .withAfter(title.getAfterTitle()).toString();
    errorResult.addReplacement(newFormattedTitle, newTitle.length() > 2);
    errors.add(errorResult);
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   *
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    final Integer namespace = analysis.getPage().getNamespace();
    if (!Objects.equals(namespace, Namespace.MAIN) && !Objects.equals(namespace, Namespace.MEDIA)) {
      return analysis.getContents();
    }
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return Array of possible global fixes.
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
