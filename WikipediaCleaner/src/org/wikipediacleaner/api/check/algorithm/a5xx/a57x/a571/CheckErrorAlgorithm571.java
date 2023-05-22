/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a571;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 571 of check wikipedia project.
 * Error 571: Article with cite tags
 */
public class CheckErrorAlgorithm571 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm571() {
    super("Article with cite tags in ref tags");
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

    // Check for <cite> tags inside <ref> tags
    boolean result = false;
    result |= analyzeCiteTags(analysis, errors);

    return result;
  }

  /**
   * Analyze a page to check if cite tags are incorrectly used.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeCiteTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementTag> citeTags = analysis.getTags(HtmlTagType.CITE);
    if ((citeTags == null) || citeTags.isEmpty()) {
      return false;
    }

    // Check each cite tag
    boolean result = false;
    for (PageElementTag citeTag : citeTags) {
      result |= analyzeCiteTag(analysis, errors, citeTag);
    }

    return result;
  }

  /**
   * Analyze a cite tag to check if it is incorrectly used.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param citeTag Cite tag to check.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeCiteTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag citeTag) {

    // Check that the cite tag is inside a ref tag
    PageElementTag refTag = analysis.getSurroundingTag(WikiTagType.REF, citeTag.getBeginIndex());
    if (refTag == null) {
      return false;
    }

    // Check for a cite tag that is a end tag
    if (citeTag.isEndTag()) {
      return analyzeCiteEndTag(analysis, errors, citeTag, refTag);
    }

    // Check for a cite tag that is not an end tag
    if (refTag.getEndIndex() != citeTag.getBeginIndex()) {
      if ((refTag.getMatchingTag() == null) ||
          (citeTag.getMatchingTag() == null)) {
        return false;
      }
      String contents = analysis.getContents();
      if ((contents.charAt(refTag.getEndIndex()) != '{') ||
          (contents.charAt(citeTag.getBeginIndex() - 1) != '}')) {
        return false;
      }
      PageElementTemplate template = analysis.isInTemplate(refTag.getEndIndex());
      if (template == null) {
        return false;
      }
    }

    if (errors == null) {
      return true;
    }

    // Handle case where cite tag go after ref tag
    boolean extended = false;
    int endIndex = citeTag.getCompleteEndIndex();
    if (endIndex >= refTag.getCompleteEndIndex()) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, citeTag.getBeginIndex(), citeTag.getEndIndex());
      errors.add(errorResult);
      return true;

    }

    // Try to extend area
    String contents = analysis.getContents();
    do {
      extended = false;
      if (endIndex < contents.length()) {
        if (contents.charAt(endIndex) == '<') {
          PageElementTag nextTag = analysis.isInTag(endIndex);
          if ((nextTag != null) && !nextTag.isFullTag() && nextTag.isComplete()) {
            if (HtmlTagType.SPAN.equals(nextTag.getType())) {
              Parameter title = nextTag.getParameter("title");
              if ((title != null) &&
                  (title.getValue() != null) &&
                  (title.getValue().startsWith("ctx_ver="))) {
                String nextTagValue = contents.substring(
                    nextTag.getValueBeginIndex(), nextTag.getValueEndIndex());
                if ((nextTagValue == null) ||
                    nextTagValue.equals("&nbsp;") ||
                    nextTagValue.equals("&#x20;")) {
                  extended = true;
                  endIndex = nextTag.getCompleteEndIndex();
                }
              }
            } else if (HtmlTagType.CITE.equals(nextTag.getType())) {
              String nextTagValue = contents.substring(
                  nextTag.getValueBeginIndex(), nextTag.getValueEndIndex());
              if ((nextTagValue == null) ||
                  nextTagValue.trim().equals("")) {
                extended = true;
                endIndex = nextTag.getCompleteEndIndex();
              }
            }
          }
        }
      }
    } while (extended);

    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, citeTag.getCompleteBeginIndex(), endIndex);
    if (contents.charAt(citeTag.getCompleteBeginIndex() - 1) == '}') {
      PageElementTemplate template = analysis.isInTemplate(citeTag.getCompleteBeginIndex() - 1);
      if ((template != null) &&
          (template.getEndIndex() == citeTag.getCompleteBeginIndex()) &&
          (template.getBeginIndex() == refTag.getValueBeginIndex())) {
        boolean canBeSafelyDeleted = false;
        String textAfterDeletion = contents.substring(refTag.getValueBeginIndex(), citeTag.getCompleteBeginIndex()).trim();
        canBeSafelyDeleted = analysis.getCompleteTags(WikiTagType.REF).stream()
            .filter(currentTag -> !Objects.equals(currentTag, refTag))
            .map(currentTag -> contents.substring(currentTag.getValueBeginIndex(), currentTag.getValueEndIndex()).trim())
            .anyMatch(currentContent -> Objects.equals(currentContent, textAfterDeletion));
        errorResult.addReplacement("", GT._T("Delete {0} tags", HtmlTagType.CITE.getOpenTag()), canBeSafelyDeleted);
      }
    }
    String replacement = contents.substring(
        citeTag.getValueBeginIndex(), citeTag.getValueEndIndex());
    if (citeTag.getCompleteEndIndex() == refTag.getValueEndIndex()) {
      replacement = replacement.trim();
    }
    errorResult.addReplacement(
        replacement,
        GT._T("Remove {0} tags", HtmlTagType.CITE.getOpenTag()));
    errors.add(errorResult);

    return true;
  }

  /**
   * Analyze a cite end tag to check if it is incorrectly used.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param citeTag Cite end tag to check.
   * @param refTag Enclosing ref tag.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeCiteEndTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag citeTag,
      PageElementTag refTag) {

    // Analyze end tags
    if (refTag.getMatchingTag() == null) {
      return false;
    }
    if (citeTag.getEndIndex() != refTag.getValueEndIndex()) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Report error
    if ((citeTag.getMatchingTag() == null) ||
        (citeTag.getMatchingTag().getBeginIndex() < refTag.getCompleteBeginIndex())) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, citeTag.getBeginIndex(), citeTag.getEndIndex());
      errors.add(errorResult);
    } else if (citeTag.getCompleteBeginIndex() > refTag.getEndIndex()) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, citeTag.getCompleteBeginIndex(), citeTag.getCompleteEndIndex());
      String replacement = analysis.getContents().substring(
          citeTag.getValueBeginIndex(), citeTag.getValueEndIndex());
      replacement = replacement.trim();
      errorResult.addReplacement(
          replacement,
          GT._T("Remove {0} tags", HtmlTagType.CITE.getOpenTag()));
      errors.add(errorResult);
    }
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
    return fixUsingAutomaticReplacement(analysis);
  }
}
