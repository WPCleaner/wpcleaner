/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a561;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;


/**
 * Algorithm for analyzing error 561 of check wikipedia project.
 * Error 561: Redirection with target text
 */
public class CheckErrorAlgorithm561 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm561() {
    super("Redirection with target text");
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

    // Check internal links
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    Page page = analysis.getPage();
    List<Page> pageLinks = page.getLinks();
    if (pageLinks == null) {
      return false;
    }
    List<Page> linkedRedirectPages = new ArrayList<>();
    for (Page linkedPage : pageLinks) {
      if ((linkedPage != null) &&
          linkedPage.getRedirects().isRedirect()) {
        linkedRedirectPages.add(linkedPage);
      }
    }
    if (linkedRedirectPages.isEmpty()) {
      return false;
    }

    // Test every internal link
    boolean result = false;
    for (PageElementInternalLink link : links) {

      // Find page matching the link
      Page linkedPage = null;
      if (link.getText() != null) {
        String fullLink = Page.normalizeTitle(link.getFullLink());
        for (Page tmpPage : linkedRedirectPages) {
          if (Page.areSameTitle(tmpPage.getTitle(), false, fullLink, true)) {
            linkedPage = tmpPage;
          }
        }
      }

      // Analyze if the text of the link matches the final target
      boolean shouldReport = false;
      if ((linkedPage != null) &&
          (linkedPage.getRedirects().isRedirect()) &&
          (Page.areSameTitle(link.getText(), linkedPage.getRedirects().getDestination()))) {
        shouldReport = true;
      }

      // Report
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        errorResult.addReplacement(InternalLinkBuilder.from(link.getText()).toString());
        errors.add(errorResult);
      }
    }
    return result;
  }
}
