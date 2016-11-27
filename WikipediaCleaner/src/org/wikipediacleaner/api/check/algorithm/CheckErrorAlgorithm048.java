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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 48 of check wikipedia project.
 * Error 48: Title linked in text
 */
public class CheckErrorAlgorithm048 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Remove all links to title (first in bold)"),
    GT._("Remove all links to title"),
  };

  public CheckErrorAlgorithm048() {
    super("Title linked in text");
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

    // Do not report redirects
    if (analysis.getPage().isRedirect()) {
      return false;
    }

    // Analyze each internal link
    boolean result = false;
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    String pageTitle = analysis.getPage().getTitle();
    String contents = analysis.getContents();
    for (PageElementInternalLink link : links) {
      // Check if it is an error
      boolean errorFound = Page.areSameTitle(pageTitle, link.getFullLink());
      if (errorFound) {
        PageElementTag tagIncludeOnly = analysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_INCLUDEONLY, link.getBeginIndex());
        if (tagIncludeOnly != null) {
          errorFound = false;
        }
      }

      // Report error
      if (errorFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        PageElementTag tagImagemap = analysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_IMAGEMAP, link.getBeginIndex());
        if (tagImagemap != null) {
          int previousCR = getPreviousCR(contents, link.getBeginIndex());
          int nextCR = getNextCR(contents, link.getEndIndex());
          nextCR = Math.min(nextCR, tagImagemap.getMatchingTag().getBeginIndex());
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, previousCR, nextCR);
          if ((previousCR > tagImagemap.getEndIndex()) &&
              (contents.charAt(nextCR) == '\n')) {
            errorResult.addReplacement("", GT._("Delete"));
          }
          errors.add(errorResult);
        } else {
          int beginIndex = link.getBeginIndex();
          int endIndex = link.getEndIndex();
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);

          // Analysis regarding titles
          boolean beforeFirstTitle = true;
          List<PageElementTitle> titles = analysis.getTitles();
          if ((titles != null) && !titles.isEmpty()) {
            PageElementTitle title = titles.get(0);
            if (title.getBeginIndex() < endIndex) {
              beforeFirstTitle = false;
            }
          }

          // Analysis regarding bold
          boolean inBold = true;
          if ((beginIndex < 3) || !contents.startsWith("'''", beginIndex - 3)) {
            inBold = false;
          } else if ((beginIndex > 3) && (contents.charAt(beginIndex - 4) == '\'')) {
            inBold = false;
          }
          if (!contents.startsWith("'''", endIndex)) {
            inBold = false;
          } else if ((contents.length() > endIndex + 4) && (contents.charAt(endIndex + 4) == '\'')) {
            inBold = false;
          }

          // Suggestions
          errorResult.addReplacement(link.getDisplayedText(), beforeFirstTitle && inBold);
          if (beforeFirstTitle && !inBold) {
            errorResult.addReplacement("'''" + link.getDisplayedText() + "'''");
          }

          errors.add(errorResult);
        }
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
    return fixUsingAutomaticReplacement(analysis);
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

    // Find first title
    String contents = analysis.getContents();
    int firstTitle = 0;
    if (fixName.equals(globalFixes[0])) {
      Collection<PageElementTitle> titles = analysis.getTitles();
      if ((titles != null) && (titles.size() > 0)) {
        firstTitle = titles.iterator().next().getBeginIndex();
      } else {
        firstTitle = contents.length();
      }
    }

    // Replace all texts
    StringBuilder newContents = new StringBuilder(contents.length());
    String pageTitle = analysis.getPage().getTitle();
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    int currentIndex = 0;
    for (PageElementInternalLink link : links) {
      if (Page.areSameTitle(pageTitle, link.getFullLink())) {
        PageElementTag tagImagemap = analysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_IMAGEMAP, link.getBeginIndex());
        if (tagImagemap != null) {
          int previousCR = getPreviousCR(contents, link.getBeginIndex());
          int nextCR = getNextCR(contents, link.getEndIndex());
          nextCR = Math.min(nextCR, tagImagemap.getMatchingTag().getBeginIndex());
          if ((previousCR > tagImagemap.getEndIndex()) &&
              (contents.charAt(nextCR) == '\n')) {
            if (previousCR > currentIndex) {
              newContents.append(contents.substring(currentIndex, previousCR));
              currentIndex = nextCR;
            }
          }
        } else {
          if (link.getBeginIndex() > currentIndex) {
            newContents.append(contents.substring(currentIndex, link.getBeginIndex()));
          }
          if ((currentIndex == 0) && (link.getBeginIndex() < firstTitle)) {
            newContents.append("'''");
            newContents.append(link.getDisplayedText());
            newContents.append("'''");
          } else {
            newContents.append(link.getDisplayedText());
          }
          currentIndex = link.getEndIndex();
        }
      }
    }
    if (currentIndex < contents.length()) {
      newContents.append(contents.substring(currentIndex));
    }
    return newContents.toString();
  }

  /**
   * Find position of previous carriage return.
   * 
   * @param contents Page contents.
   * @param currentIndex Current index.
   * @return Index of previous carriage return.
   */
  private int getPreviousCR(String contents, int currentIndex) {
    if (contents == null) {
      return 0;
    }
    int tmpIndex = currentIndex - 1;
    while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) != '\n')) {
      tmpIndex--;
    }
    return Math.max(0, tmpIndex);
  }

  /**
   * Find position of next carriage return.
   * 
   * @param contents Page contents.
   * @param currentIndex Current index.
   * @return Index of next carriage return.
   */
  private int getNextCR(String contents, int currentIndex) {
    if (contents == null) {
      return -1;
    }
    int tmpIndex = currentIndex;
    while ((tmpIndex < contents.length()) &&
           (contents.charAt(tmpIndex) != '\n')) {
      tmpIndex++;
    }
    return tmpIndex;
  }
}
