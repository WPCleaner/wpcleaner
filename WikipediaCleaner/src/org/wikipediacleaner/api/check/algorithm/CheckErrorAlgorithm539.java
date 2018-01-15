/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 539 of check wikipedia project.
 * Error 539: Misnested tags (see [[Special:LintErrors/misnested-tag]])
 */
public class CheckErrorAlgorithm539 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm539() {
    super("Misnested tags");
  }

  /** Possible replacements */
  private final static Replacement[] replacements = {
    new Replacement(
        PageElementTag.TAG_HTML_BIG,
        new ReplacementElement[] {
          new ReplacementElement(PageElementTag.TAG_HTML_CENTER, true, Order.MUST_INVERT),
        }),
    new Replacement(
        PageElementTag.TAG_HTML_CENTER,
        new ReplacementElement[] {
          new ReplacementElement(PageElementTag.TAG_HTML_BIG, true, Order.MUST_KEEP),
          new ReplacementElement(PageElementTag.TAG_HTML_SMALL, true, Order.MUST_KEEP),
        }),
    new Replacement(
        PageElementTag.TAG_HTML_DIV,
        new ReplacementElement[] {
          new ReplacementElement(PageElementTag.TAG_HTML_BIG, true, Order.MUST_KEEP),
          new ReplacementElement(PageElementTag.TAG_HTML_CENTER, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(PageElementTag.TAG_HTML_SMALL, true, Order.MUST_KEEP),
        }),
    new Replacement(
        PageElementTag.TAG_HTML_FONT,
          new ReplacementElement[] {
          new ReplacementElement(PageElementTag.TAG_HTML_SUP, true, Order.BOTH_POSSIBLE),
        }),
    new Replacement(
        PageElementTag.TAG_HTML_SMALL,
        new ReplacementElement[] {
          new ReplacementElement(PageElementTag.TAG_HTML_CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(PageElementTag.TAG_HTML_FONT, true, Order.BOTH_POSSIBLE),
        }),
    new Replacement(
        PageElementTag.TAG_HTML_SPAN,
        new ReplacementElement[] {
          new ReplacementElement(PageElementTag.TAG_HTML_BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(PageElementTag.TAG_HTML_SMALL, true, Order.BOTH_POSSIBLE),
        }),
  };

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

    // Analyze each type of tag
    boolean result = false;
    for (Replacement replacement : replacements) {
      result |= analyzeTags(analysis, errors, replacement);
    }

    // Analyze each internal link
    List<PageElementInternalLink> iLinks = analysis.getInternalLinks();
    for (PageElementInternalLink iLink : iLinks) {
      result |= analyzeInternalLink(analysis, errors, iLink);
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link to be analyzed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Preliminary check
    if ((link == null) || (link.getText() == null)) {
      return false;
    }

    // Analyze tags in the internal link
    String contents = analysis.getContents();
    int beginIndex = link.getBeginIndex() + link.getTextOffset();
    int endIndex = link.getEndIndex() - 2;
    int index = endIndex;
    while (index > beginIndex) {
      if (contents.charAt(index - 1) == '>') {
        PageElementTag tag = analysis.isInTag(index -1);
        if (tag != null) {
          if ((tag.getEndIndex() == index) &&
              (tag.isComplete()) &&
              (!tag.isFullTag()) &&
              (!tag.isEndTag())) {
            if (tag.getCompleteEndIndex() > endIndex) {
              if (errors == null) {
                return true;
              }

              // Check if the closing tag is just after the link
              PageElementTag closingTag = tag.getMatchingTag();
              int tmpIndex = link.getEndIndex();
              while ((tmpIndex < closingTag.getBeginIndex())&&
                     (" \n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
                tmpIndex++;
              }
              if (tmpIndex < closingTag.getBeginIndex()) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, tag.getBeginIndex(), tag.getEndIndex());
                errors.add(errorResult);
              } else {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
                String replacement =
                    contents.substring(tag.getCompleteBeginIndex(), link.getEndIndex() - 2) +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex()) +
                    contents.substring(link.getEndIndex() - 2, tag.getValueEndIndex());
                String text =
                    contents.substring(tag.getCompleteBeginIndex(), tag.getValueBeginIndex()) +
                    "..." +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex()) +
                    contents.substring(link.getEndIndex() - 2, tag.getValueEndIndex());
                errorResult.addReplacement(replacement, text, true);
                errors.add(errorResult);
              }
              return true;
            }
          }
          index = tag.getBeginIndex();
        } else {
          index--;
        }
      } else {
        index--;
      }
    }

    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param replacement Possible replacement.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTags(
    PageAnalysis analysis,
    Collection<CheckErrorResult> errors,
    Replacement replacement) {

    // Analyze each tag
    boolean result = false;
    List<PageElementTag> tags = analysis.getCompleteTags(replacement.firstTag);
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, replacement, tag);
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param replacement Possible replacement.
   * @param tag Tag currently being analyzed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      Replacement replacement,
      PageElementTag tag) {

    // Ignore whitespace characters at the beginning
    String contents = analysis.getContents();
    int index = tag.getValueBeginIndex();
    while ((index < tag.getValueEndIndex()) &&
           (" \n".indexOf(contents.charAt(index)) >= 0)) {
      index++;
    }

    // Analyze area
    while (index < tag.getValueEndIndex()) {

      // Look for an other tag
      PageElementTag internalTag = null;
      if (contents.charAt(index) == '<') {
        internalTag = analysis.isInTag(index);
      }

      // Analyze tag
      if (internalTag != null) {
        if (internalTag.isComplete() &&
            (internalTag.getCompleteEndIndex() > tag.getCompleteEndIndex())) {
          if (errors == null) {
            return true;
          }

          // Analyze if a replacement can be suggested
          ReplacementElement element = replacement.getSecondTag(internalTag.getNormalizedName());

          // Report error
          if (element != null) {

            // Try with keeping order
            if (element.order.canKeepOrder()) {
              int tmpIndex = tag.getCompleteEndIndex();
              while ((tmpIndex < internalTag.getValueEndIndex()) &&
                     (" \n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
                tmpIndex++;
              }
              if (tmpIndex >= internalTag.getValueEndIndex()) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, internalTag.getCompleteBeginIndex(), internalTag.getCompleteEndIndex());
                String text =
                    contents.substring(internalTag.getCompleteBeginIndex(), tag.getValueEndIndex()) +
                    contents.substring(tag.getCompleteEndIndex(), internalTag.getCompleteEndIndex()) +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex());
                String desc =
                    contents.substring(internalTag.getCompleteBeginIndex(), internalTag.getValueBeginIndex()) +
                    "..." +
                    contents.substring(internalTag.getValueEndIndex(), internalTag.getCompleteEndIndex()) +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex());
                errorResult.addReplacement(text,  desc, element.automatic);
                errors.add(errorResult);
                return true;
              }
            }

            // Try with inverting order
            if (element.order.canInvertOrder()) {
              int tmpIndex = tag.getValueBeginIndex();
              while ((tmpIndex < internalTag.getCompleteBeginIndex()) &&
                     (" \n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
                tmpIndex++;
              }
              if (tmpIndex >= internalTag.getCompleteBeginIndex()) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
                String text =
                    contents.substring(internalTag.getCompleteBeginIndex(), internalTag.getValueBeginIndex()) +
                    contents.substring(tag.getCompleteBeginIndex(), internalTag.getCompleteBeginIndex()) +
                    contents.substring(internalTag.getValueBeginIndex(), tag.getCompleteEndIndex());
                String desc =
                    contents.substring(internalTag.getCompleteBeginIndex(), internalTag.getValueBeginIndex()) +
                    contents.substring(tag.getCompleteBeginIndex(), tag.getValueBeginIndex()) +
                    "..." +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex());
                errorResult.addReplacement(text, desc, element.automatic);
                errors.add(errorResult);
                return true;
              }
            }
          }

          // Default reporting
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, internalTag.getBeginIndex(), internalTag.getEndIndex());
          errors.add(errorResult);
          return true;
        }
        index = internalTag.getEndIndex();
      } else {
        index++;
      }
    }

    return false;
  }

  /**
   * Bean for holding configuration for replacements.
   */
  private static class Replacement {

    /** First tag: surrounding */
    final String firstTag;

    /** Second tag: should be inside */
    final List<ReplacementElement> elements;

    /**
     * @param firstTag Surrounding tag.
     * @param secondTags Tags that should be inside.
     * @param automatic Automatic replacement.
     */
    Replacement(
        String firstTag,
        ReplacementElement[] elements) {
      this.firstTag = firstTag;
      this.elements = Arrays.asList(elements);
    }

    ReplacementElement getSecondTag(String tagName) {
      for (ReplacementElement element : elements) {
        if (element.tag.equals(tagName)) {
          return element;
        }
      }
      return null;
    }
  }

  /**
   * Bean for holding configuration for a replacement.
   */
  private static class ReplacementElement {

    /** Tag */
    final String tag;

    /** True if replacement can be automatic */
    final boolean automatic;

    /** Possibilities for order of tags */
    final Order order;

    /**
     * @param tag Included tag.
     * @param automatic Automatic replacement.
     * @order Possibilities for order of tags.
     */
    ReplacementElement(
        String tag,
        boolean automatic,
        Order order) {
      this.tag = tag;
      this.automatic = automatic;
      this.order = order;
    }
  }

  /**
   * Enumeration for possible order changes.
   */
  private static enum Order {
    MUST_KEEP,
    BOTH_POSSIBLE,
    MUST_INVERT;

    /**
     * @return True if tags order can be keep.
     */
    boolean canKeepOrder() {
      if ((this == MUST_KEEP) || (this == BOTH_POSSIBLE)) {
        return true;
      }
      return false;
    }

    /**
     * @return True if tags order can be inverted.
     */
    boolean canInvertOrder() {
      if ((this == MUST_INVERT) || (this == BOTH_POSSIBLE)) {
        return true;
      }
      return false;
    }
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}