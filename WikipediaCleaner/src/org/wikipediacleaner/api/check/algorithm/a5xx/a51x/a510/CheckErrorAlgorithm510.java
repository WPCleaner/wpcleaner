/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a51x.a510;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 510 of check wikipedia project.
 * Error 510: Non-working pipe trick
 */
@SuppressWarnings("unused")
public class CheckErrorAlgorithm510 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm510() {
    super("Non-working pipe trick");
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
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each internal link
    boolean result = false;
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    for (PageElementInternalLink link : links) {
      result |= analyzeInternalLink(analysis, errors, link);
    }
    return result;
  }

  private boolean analyzeInternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {
    if ((link.getText() == null) ||
        (!link.getText().isEmpty()) ||
        (link.getFullLink() == null)) {
      return false;
    }

    String target = link.getFullLinkNotNormalized().trim();
    if ((analysis.getSurroundingTag(WikiTagType.GALLERY, link.getBeginIndex()) != null) ||
        (analysis.getSurroundingTag(WikiTagType.REF, link.getBeginIndex()) != null)) {

      // Check for namespace at the beginning
      int beginIndex = 0;
      if (target.length() > 1) {
        int tmpIndex = target.indexOf(':');
        if (tmpIndex > 0) {
          beginIndex = tmpIndex + 1;
        }
      }

      // Check for parenthesis or commas (remove the end part)
      int endIndex = target.length();
      if (endIndex > 0) {
        if (target.charAt(endIndex - 1) == ')') {
          int tmpIndex = target.lastIndexOf('(');
          if (tmpIndex > 0) {
            endIndex = tmpIndex;
          }
        }
        if (endIndex == target.length()) {
          int tmpIndex = target.indexOf(',');
          if (tmpIndex > 0) {
            endIndex = tmpIndex;
          }
        }
      }

      // Report error
      if (errors == null) {
        return true;
      }
      if (beginIndex >= endIndex) {
        beginIndex = 0;
      }
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, link.getBeginIndex(), link.getEndIndex());
      String replacement = InternalLinkBuilder
          .from(target)
          .withText(target.substring(beginIndex, endIndex))
          .toString();
      errorResult.addReplacement(replacement, true);
      replacement = InternalLinkBuilder.from(target).toString();
      errorResult.addReplacement(replacement);
      errors.add(errorResult);
      return true;
    }

    // Incorrect slash trick
    int endIndex = target.length();
    if ((endIndex > 1) && (target.charAt(0) == '/')) {
      if (errors == null) {
        return true;
      }
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, link.getBeginIndex(), link.getEndIndex());
      String replacement = InternalLinkBuilder.from(target + "/").toString();
      errorResult.addReplacement(replacement);
      replacement = InternalLinkBuilder.from(target).toString();
      errorResult.addReplacement(replacement);
      errors.add(errorResult);
      return true;
    }

    // Link to section
    int index = target.indexOf('#');
    if ((index >= 0) && (index < target.length() - 1)) {
      if (errors == null) {
        return true;
      }
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, link.getBeginIndex(), link.getEndIndex());
      String replacement = InternalLinkBuilder
          .from(target)
          .withText(target.substring(index + 1))
          .toString();
      errorResult.addReplacement(replacement);
      replacement = InternalLinkBuilder.from(target).toString();
      errorResult.addReplacement(replacement);
      errors.add(errorResult);
      return true;
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
    if (!analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
