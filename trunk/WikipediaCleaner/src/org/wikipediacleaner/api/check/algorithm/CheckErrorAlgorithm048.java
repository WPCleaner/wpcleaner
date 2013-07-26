/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

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
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    boolean result = false;
    Collection<PageElementInternalLink> links = pageAnalysis.getInternalLinks();
    String pageTitle = pageAnalysis.getPage().getTitle();
    String contents = pageAnalysis.getContents();
    for (PageElementInternalLink link : links) {
      if (Page.areSameTitle(pageTitle, link.getFullLink())) {
        if (errors == null) {
          return true;
        }
        result = true;
        PageElementTag tagImagemap = pageAnalysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_IMAGEMAP, link.getBeginIndex());
        if (tagImagemap != null) {
          int previousCR = getPreviousCR(contents, link.getBeginIndex());
          int nextCR = getNextCR(contents, link.getEndIndex());
          nextCR = Math.min(nextCR, tagImagemap.getMatchingTag().getBeginIndex());
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), previousCR, nextCR);
          if ((previousCR > tagImagemap.getEndIndex()) &&
              (contents.charAt(nextCR) == '\n')) {
            errorResult.addReplacement("", GT._("Delete"));
          }
          errors.add(errorResult);
        } else {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              link.getBeginIndex(),
              link.getEndIndex());
          errorResult.addReplacement(link.getDisplayedText());
          errorResult.addReplacement("'''" + link.getDisplayedText() + "'''");
          errors.add(errorResult);
        }
      }
    }
    return result;
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
