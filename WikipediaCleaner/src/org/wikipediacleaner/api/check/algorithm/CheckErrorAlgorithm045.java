/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 45 of check wikipedia project.
 * Error 45: Interwiki double
 */
public class CheckErrorAlgorithm045 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Delete all"),
  };

  public CheckErrorAlgorithm045() {
    super("Interwiki double");
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


    // Group language links by index
    List<PageElementLanguageLink> links = analysis.getLanguageLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    Map<String, List<PageElementLanguageLink>> groupedLinks = new HashMap<String, List<PageElementLanguageLink>>();
    for (PageElementLanguageLink link : links) {
      String index = link.getLanguage() + ":" + link.getLink();
      List<PageElementLanguageLink> groupLink = groupedLinks.get(index);
      if (groupLink == null) {
        groupLink = new ArrayList<PageElementLanguageLink>();
        groupedLinks.put(index, groupLink);
      }
      groupLink.add(link);
    }

    // Compute index of last index
    List<PageElementTitle> titles = analysis.getTitles();
    int lastIndex = 0;
    if ((titles != null) && (!titles.isEmpty())) {
      lastIndex = titles.get(titles.size() - 1).getEndIndex();
    }
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories != null) && (!categories.isEmpty())) {
      lastIndex = Math.max(lastIndex, categories.get(categories.size() - 1).getEndIndex());
    }

    // Check each language link
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementLanguageLink link : links) {
      String index = link.getLanguage() + ":" + link.getLink();
      List<PageElementLanguageLink> groupLink = groupedLinks.get(index);
      if ((groupLink != null) && (groupLink.size() > 1)) {
        if (errors == null) {
          return true;
        }
        result = true;
        PageElementLanguageLink keepLink = keepLanguageLink(groupLink, lastIndex);
        if (keepLink == link) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              link.getBeginIndex(),
              link.getEndIndex(),
              CheckErrorResult.ErrorLevel.CORRECT);
          errors.add(errorResult);
        } else {
          int beginIndex = link.getBeginIndex();
          while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == ' ')) {
            beginIndex--;
          }
          boolean beginLine = (beginIndex == 0) || (contents.charAt(beginIndex - 1) == '\n');
          int endIndex = link.getEndIndex();
          while ((endIndex < contents.length()) && (contents.charAt(endIndex) == ' ')) {
            endIndex++;
          }
          boolean endLine = (endIndex >= contents.length()) || (contents.charAt(endIndex) == '\n');
          if (beginLine && endLine) {
            endIndex = Math.min(endIndex + 1, contents.length());
          }
          if (!beginLine) {
            beginIndex = link.getBeginIndex();
          }
          if (!endLine) {
            endIndex = link.getEndIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
          errorResult.addReplacement("", GT._T("Delete"));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param links List of language links for the same index.
   * @param lastIndex Index of the last title / last category.
   * @return Which language link should be kept.
   */
  private PageElementLanguageLink keepLanguageLink(
      List<PageElementLanguageLink> links, int lastIndex) {

    // First: language link after last index
    for (PageElementLanguageLink link : links) {
      if (link.getBeginIndex() >= lastIndex) {
        return link;
      }
    }

    // Last: last language link
    return links.get(links.size() - 1);
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
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
