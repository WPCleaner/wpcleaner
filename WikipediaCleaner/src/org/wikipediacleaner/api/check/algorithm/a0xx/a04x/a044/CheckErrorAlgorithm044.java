/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a04x.a044;

import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;
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
    GT._T("Fix all bold headlines"),
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
      result |= analyzeTitle(analysis, errors, title);
    }

    return result;
  }

  /**
   * Analyze a title to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param title Title to be analyzed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTitle(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTitle title) {

    // Check if the title has bold
    String contents = analysis.getContents();
    int index = title.getBeginIndex();
    int countBold = 0;
    int countItalic = 0;
    while (index < title.getEndIndex()) {
      if (contents.startsWith("<", index)) {
        PageElementTag tag = analysis.isInTag(index, WikiTagType.REF);
        if ((tag != null) && (tag.getBeginIndex() == index)) {
          index = tag.getCompleteEndIndex();
        } else {
          ContentsComment comment = analysis.comments().getBeginsAt(index);
          if (comment != null) {
            index = comment.getEndIndex();
          } else {
            index++;
          }
        }
      } else if (contents.startsWith("'''", index)) {
        index += 3;
        countBold++;
      } else if (contents.startsWith("''", index)) {
        index += 2;
        countItalic = 1;
      } else {
        index++;
      }
    }

    // Check if error is present
    if (countBold == 0) {
      return false;
    }
    if ((countBold == 1) && (countItalic % 2 == 1)) {
      return false;
    }

    // Register error
    if (errors == null) {
      return true;
    }
    String text = StringUtils.defaultString(title.getTitle(), "").trim();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        title.getBeginIndex(), title.getEndIndex());
    if (countBold >= 2) {
      boolean automaticBot = false;
      if (countBold == 2) {
        String tmpText = text;
        int currentIndex = title.getAfterTitleIndex();
        currentIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, currentIndex - 1, "=");
        currentIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, currentIndex, " ");
        currentIndex++;
        if ((currentIndex > 0) && (contents.charAt(currentIndex - 1) == '>')) {
          PageElementTag tag = analysis.isInTag(currentIndex - 1, WikiTagType.REF);
          if ((tag != null) && (tag.getCompleteEndIndex() == currentIndex)) {
            tmpText = tmpText.substring(0, Math.max(0, tmpText.length() - tag.getCompleteEndIndex() + tag.getCompleteBeginIndex()));
          }
        }
        if (tmpText.startsWith("'''") && tmpText.endsWith("'''")) {
          automaticBot = true;
        }
      }
      errorResult.addReplacement(TitleBuilder
          .from(title.getLevel(), text.replaceAll("'''", ""))
          .withAfter(title.getAfterTitle()).toString(),
          false, automaticBot);
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
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
