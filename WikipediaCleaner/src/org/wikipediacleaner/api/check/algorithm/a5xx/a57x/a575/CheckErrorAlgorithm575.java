/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a575;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;


/**
 * Algorithm for analyzing error 575 of check wikipedia project.
 * <br>
 * Error 575: Non-breaking space in internal links.
 */
public class CheckErrorAlgorithm575 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm575() {
    super("Non-breaking space in internal links");
  }

  private static final List<String> NBSP = Stream.of("&nbsp;", "\u00A0").collect(Collectors.toList());

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

    // Check each internal link
    List<PageElementInternalLink> links  = analysis.getInternalLinks();
    if ((links == null) || links.isEmpty()) {
      return false;
    }
    boolean result = false;
    for (PageElementInternalLink link : links) {
      result |= analyzeInternalLink(analysis, errors, link);
    }

    return result;
  }

  /**
   * Analyze an internal link to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Check that the link text is not empty
    String text = link.getText();
    if ((text == null) || text.isEmpty()) {
      return false;
    }

    // Check the end of the link text
    for (String nbsp : NBSP) {
      if (text.endsWith(nbsp)) {
        String contents = analysis.getContents();
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        int fullBeginIndex = Math.max(0, beginIndex - 2);
        int fullEndIndex = Math.min(endIndex + 2, contents.length());
        CheckErrorResult error = createCheckErrorResult(analysis, fullBeginIndex, fullEndIndex);
        if (text.length() > nbsp.length()) {
          String newText = text.substring(0, text.length() - nbsp.length());
          String replacement =
              contents.substring(fullBeginIndex, beginIndex) +
              InternalLinkBuilder.from(link.getLink()).withAnchor(link.getAnchor()).withText(newText).toString() +
              nbsp +
              contents.substring(endIndex, fullEndIndex);
          error.addReplacement(replacement, true);
        } else {
          error.addReplacement(nbsp, true);
        }
        errors.add(error);
        return true;
      }
    }

    return false;
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
    return fixUsingAutomaticReplacement(analysis);
  }
}
