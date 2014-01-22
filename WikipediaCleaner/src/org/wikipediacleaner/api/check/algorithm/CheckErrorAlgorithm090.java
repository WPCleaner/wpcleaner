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
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 090 of check wikipedia project.
 * Error 090: Internal link written as external link
 */
public class CheckErrorAlgorithm090 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Convert them to internal links"),
  };

  public CheckErrorAlgorithm090() {
    super("Internal link written as external link");
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
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    WikiConfiguration wikiConf = wiki.getWikiConfiguration();
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      if (link.hasSquare()) {
        String article = wikiConf.isArticleUrl(link.getLink());
        if ((article != null) && (article.length() > 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          int beginIndex = link.getBeginIndex();
          int endIndex = link.getEndIndex();
          if ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '[') &&
              (endIndex < contents.length()) && (contents.charAt(endIndex) == ']')) {
            beginIndex--;
            endIndex++;
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), beginIndex, endIndex);
          errorResult.addReplacement(PageElementInternalLink.createInternalLink(article, link.getText()));
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
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
