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
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 515 of check wikipedia project.
 * Error 515: Title linked in text through redirect link
 */
public class CheckErrorAlgorithm515 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm515() {
    super("Title linked in text through redirect link");
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

    // Preliminary setup
    Page page = analysis.getPage();
    String pageTitle = page.getTitle();
    String contents = analysis.getContents();
    List<Page> linkedPages = page.getLinks();
    if ((pageTitle == null) || (contents == null) || (linkedPages == null)) {
      return false;
    }
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }

    // Test every internal link
    boolean result = false;
    for (PageElementInternalLink link : links) {

      // Find page matching the link
      Page linkedPage = null;
      for (Page tmpPage : linkedPages) {
        if (Page.areSameTitle(tmpPage.getTitle(), link.getFullLink())) {
          linkedPage = tmpPage;
        }
      }

      // Check if the link is circular
      if ((linkedPage != null) &&
          linkedPage.isRedirect() &&
          Page.areSameTitle(pageTitle, linkedPage.getRedirectTitle())) {
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
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
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
