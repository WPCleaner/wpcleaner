/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a539;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFormatting;
import org.wikipediacleaner.api.data.PageElementFormattingAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


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
        HtmlTagType.BIG,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.CENTER,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.DIV, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.FONT, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.S, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUB, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUP, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.U, true, Order.MUST_KEEP),
        },
        OrderFormatting.FORMATTING_INSIDE),
    new Replacement(
        HtmlTagType.DIV,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.FONT, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.S, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUB, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.SUP, true, Order.MUST_KEEP),
          new ReplacementElement(HtmlTagType.U, true, Order.MUST_KEEP),
        },
        OrderFormatting.FORMATTING_INSIDE),
    new Replacement(
        HtmlTagType.FONT,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.S,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SMALL,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SPAN,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SUB,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.SUP,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.U, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
    new Replacement(
        HtmlTagType.U,
        new ReplacementElement[] {
          new ReplacementElement(HtmlTagType.BIG, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.CENTER, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.DIV, true, Order.MUST_INVERT),
          new ReplacementElement(HtmlTagType.FONT, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.S, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SMALL, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SPAN, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUB, true, Order.BOTH_POSSIBLE),
          new ReplacementElement(HtmlTagType.SUP, true, Order.BOTH_POSSIBLE),
        },
        OrderFormatting.FORMATTING_ANYWHERE),
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

    // Analyze each external link
    List<PageElementExternalLink> eLinks = analysis.getExternalLinks();
    for (PageElementExternalLink eLink : eLinks) {
      result |= analyzeExternalLink(analysis, errors, eLink);
    }

    // Analyze formatting elements
    result |= analyzeFormattingElements(analysis, errors);

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeFormattingElements(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Analyze contents to find formatting elements
    boolean result = false;
    List<PageElementFormatting> elements = PageElementFormatting.listFormattingElements(analysis);
    if (elements.isEmpty()) {
      return false;
    }

    // Analyze inside tags
    for (Replacement replacement : replacements) {
      List<PageElementTag> tags = analysis.getCompleteTags(replacement.firstTag);
      for (PageElementTag tag : tags) {
        result |= analyzeForFormattingElements(
            analysis, errors,
            tag.getCompleteBeginIndex(), tag.getCompleteEndIndex(),
            tag.getValueBeginIndex(), tag.getValueEndIndex(),
            replacement.orderFormatting, elements);
      }
    }

    // Analyze inside internal links
    List<PageElementInternalLink> iLinks = analysis.getInternalLinks();
    for (PageElementInternalLink iLink : iLinks) {
      if (iLink.getTextOffset() > 0) {
        result |= analyzeForFormattingElements(
            analysis, errors,
            iLink.getBeginIndex(), iLink.getEndIndex(),
            iLink.getBeginIndex() + iLink.getTextOffset(), iLink.getEndIndex() - 2,
            OrderFormatting.FORMATTING_ANYWHERE, elements);
      }
    }

    // Analyze inside external links
    List<PageElementExternalLink> eLinks = analysis.getExternalLinks();
    for (PageElementExternalLink eLink : eLinks) {
      if (eLink.hasSquare() && eLink.hasSecondSquare() && (eLink.getTextOffset() > 0)) {
        result |= analyzeForFormattingElements(
            analysis, errors,
            eLink.getBeginIndex(), eLink.getEndIndex(),
            eLink.getBeginIndex() + eLink.getTextOffset(), eLink.getEndIndex() - 1,
            OrderFormatting.FORMATTING_ANYWHERE, elements);
      }
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Tag.
   * @param replacement Replacement configuration.
   * @param elements Formatting elements.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeForFormattingElements(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int externalBegin, int externalEnd,
      int internalBegin, int internalEnd,
      OrderFormatting orderFormatting,
      List<PageElementFormatting> elements) {

    // Only deal with areas containing one formatting element
    PageElementFormattingAnalysis formatting = PageElementFormattingAnalysis.analyzeArea(
        elements, internalBegin, internalEnd);
    if (formatting.getElements().size() != 1) {
      return false;
    }

    // Check that the area contains a second formatting element
    PageElementFormatting element = formatting.getElements().get(0);
    int areaBegin = element.getMainAreaBegin();
    int areaEnd = element.getMainAreaEnd();
    String contents = analysis.getContents();
    if ((areaBegin == 0) && (areaEnd == contents.length())) {
      return false;
    }
    PageElementFormattingAnalysis areaFormatting = PageElementFormattingAnalysis.analyzeArea(
        elements, areaBegin, areaEnd);
    if (areaFormatting.getElements().size() != 2) {
      return false;
    }
    int elementBegin = element.getIndex();
    int elementEnd = element.getIndex() + element.getLength();

    // Check where the element is
    int tmpIndex = elementBegin;
    while ((tmpIndex > internalBegin) &&
           (contents.charAt(tmpIndex - 1) == ' ')) {
      tmpIndex--;
    }
    boolean atBeginning = (tmpIndex == internalBegin);
    tmpIndex = elementEnd;
    while ((tmpIndex < internalEnd) &&
           (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }
    boolean atEnd = (tmpIndex == internalEnd);
    if (!atBeginning && !atEnd) {
      return false;
    }

    // Determine the other formatting element
    PageElementFormatting otherElement = areaFormatting.getElements().get(0);
    if (otherElement == element) {
      otherElement = areaFormatting.getElements().get(1);
    }
    if (otherElement.getLength() != element.getLength()) {
      return false;
    }
    int otherBegin = otherElement.getIndex();
    int otherEnd = otherElement.getIndex() + otherElement.getLength();
    if ((otherBegin >= externalBegin) && (otherEnd <= externalEnd)) {
      return false;
    }
    boolean otherAfter = (otherElement.getIndex() > internalEnd);
    boolean otherClose = false;
    if (otherAfter) {
      tmpIndex = otherBegin;
      while ((tmpIndex > externalEnd) &&
             (contents.charAt(tmpIndex - 1) == ' ')) {
        tmpIndex--;
      }
      otherClose = (tmpIndex == externalEnd);
    } else {
      tmpIndex = otherElement.getIndex() + otherElement.getLength();
      while ((tmpIndex < externalBegin) &&
             (contents.charAt(tmpIndex) == ' ')) {
        tmpIndex++;
      }
      otherClose = (tmpIndex == externalBegin);
    }

    // Manage when element is at the beginning of the tag
    if (atBeginning) {
      if (otherAfter) {

        // Other tag is just after and can be placed inside
        if (orderFormatting.canBeInside() && otherClose) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, internalEnd, otherEnd);
          errorResult.addReplacement(
              contents.substring(otherBegin, otherEnd) + contents.substring(internalEnd, otherBegin),
              true);
          errors.add(errorResult);
          return true;
        }

        // Other tag is after but first tag can be put outside
        if (orderFormatting.canBeOutside()) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, externalBegin, elementEnd);
          errorResult.addReplacement(
              contents.substring(elementBegin, elementEnd) + contents.substring(externalBegin, elementBegin),
              true);
          errors.add(errorResult);
          return true;
        }
      } else {

        // Other tag is before
        if (!otherClose) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, externalBegin, elementEnd);
          errorResult.addReplacement(
              contents.substring(elementBegin, elementEnd) + contents.substring(externalBegin, elementBegin),
              true);
          errors.add(errorResult);
          return true;
        }
      }
    }

    // Manage when element is at the end of the tag
    if (atEnd) {
      if (!otherAfter) {

        // Other tag is before but second tag can be put outside
        if (orderFormatting.canBeOutside() && !otherClose) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, elementBegin, externalEnd);
          errorResult.addReplacement(
              contents.substring(elementEnd, externalEnd) + contents.substring(elementBegin, elementEnd),
              true);
          errors.add(errorResult);
          return true;
        }

        // Other tag is just before and can be placed inside
        if (orderFormatting.canBeInside() && otherClose) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, otherBegin, internalBegin);
          errorResult.addReplacement(
              contents.substring(otherEnd, internalBegin) + contents.substring(otherBegin, otherEnd),
              true);
          errors.add(errorResult);
          return true;
        }
      } else {

        // Other tag is after
        if (!otherClose) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, elementBegin, externalEnd);
          errorResult.addReplacement(
              contents.substring(elementEnd, externalEnd) + contents.substring(elementBegin, elementEnd),
              true);
          errors.add(errorResult);
          return true;
        }
      }
    }

    return false;
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
    if ((link == null) ||
        (link.getText() == null)) {
      return false;
    }
    return analyzeLink(
        analysis, errors, link,
        link.getBeginIndex() + link.getTextOffset(),
        link.getEndIndex() - 2);
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link to be analyzed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeExternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementExternalLink link) {

    // Preliminary check
    if ((link == null) ||
        (link.getText() == null) ||
        (!link.hasSquare()) ||
        (!link.hasSecondSquare())) {
      return false;
    }
    return analyzeLink(
        analysis, errors, link,
        link.getBeginIndex() + link.getTextOffset(),
        link.getEndIndex() - 1);
  }

  /**
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Link to be analyzed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElement link, final int beginIndex, final int endIndex) {

    // Analyze tags in the internal link
    String contents = analysis.getContents();
    int index = endIndex;
    while (index > beginIndex) {
      if (contents.charAt(index - 1) == '>') {
        PageElementTag tag = analysis.isInTag(index -1);
        if (tag != null) {
          boolean shouldCheckTag = false;
          if ((tag.getEndIndex() == index) &&
              (tag.isComplete()) &&
              (!tag.isFullTag()) &&
              (!tag.isEndTag())) {
            shouldCheckTag = true;
          }
          if (shouldCheckTag) {
            if (WikiTagType.NOWIKI.equals(tag.getType()) ||
                HtmlTagType.CODE.equals(tag.getType())) {
              shouldCheckTag = false;
            }
          }
          if (shouldCheckTag) {
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
                    contents.substring(tag.getCompleteBeginIndex(), endIndex) +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex()) +
                    contents.substring(endIndex, tag.getValueEndIndex());
                String text =
                    contents.substring(tag.getCompleteBeginIndex(), tag.getValueBeginIndex()) +
                    "..." +
                    contents.substring(tag.getValueEndIndex(), tag.getCompleteEndIndex()) +
                    contents.substring(endIndex, tag.getValueEndIndex());
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

    // Analyze area for other tags
    while (index < tag.getValueEndIndex()) {

      // Look for an other tag
      PageElementTag internalTag = null;
      if (contents.charAt(index) == '<') {
        internalTag = analysis.isInTag(index);
      }

      // Analyze tag
      if (internalTag != null) {

        // Analyze if error should be reported
        boolean reportError = false;
        if (internalTag.isComplete() &&
            (internalTag.getCompleteEndIndex() > tag.getCompleteEndIndex())) {
          if ((analysis.getSurroundingTag(WikiTagType.SOURCE, index) == null) &&
              (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, index) == null)) {
            reportError = true;
          }
        }

        // Report error
        if (reportError) {
          if (errors == null) {
            return true;
          }

          // Analyze if a replacement can be suggested
          ReplacementElement element = replacement.getSecondTag(internalTag.getType());

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
    final TagType firstTag;

    /** Second tag: should be inside */
    final List<ReplacementElement> elements;

    /** Formatting order */
    final OrderFormatting orderFormatting;

    /**
     * @param firstTag Surrounding tag type.
     * @param elements Possible tags inside.
     * @param orderFormatting Formatting order.
     */
    Replacement(
        TagType firstTag,
        ReplacementElement[] elements,
        OrderFormatting orderFormatting) {
      this.firstTag = firstTag;
      this.elements = Arrays.asList(elements);
      this.orderFormatting = orderFormatting;
    }

    ReplacementElement getSecondTag(TagType tag) {
      for (ReplacementElement element : elements) {
        if (element.tag.equals(tag)) {
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
    final TagType tag;

    /** True if replacement can be automatic */
    final boolean automatic;

    /** Possibilities for order of tags */
    final Order order;

    /**
     * @param tagType Included tag type.
     * @param automatic Automatic replacement.
     * @order Possibilities for order of tags.
     */
    ReplacementElement(
        @Nonnull TagType tagType,
        boolean automatic,
        Order order) {
      this.tag = tagType;
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
   * Enumeration for possible order with formatting elements.
   */
  private static enum OrderFormatting {
    FORMATTING_INSIDE,
    FORMATTING_OUTSIDE,
    FORMATTING_ANYWHERE;

    /**
     * @return True if formatting can be inside.
     */
    boolean canBeInside() {
      if ((this == FORMATTING_INSIDE) || (this == FORMATTING_ANYWHERE)) {
        return true;
      }
      return false;
    }

    /**
     * @return True if formatting can be outside.
     */
    boolean canBeOutside() {
      if ((this == FORMATTING_OUTSIDE) || (this == FORMATTING_ANYWHERE)) {
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

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (linterCategory != null);
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    List<Page> result = null;
    if (linterCategory != null) {
      API api = APIFactory.getAPI();
      try {
        result = api.retrieveLinterCategory(
            wiki, linterCategory.getCategory(),
            Namespace.MAIN, false, true, limit);
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    List<LinterCategory> categories = getWikiConfiguration().getLinterCategories();
    if (categories != null) {
      for (LinterCategory category : categories) {
        if ("misnested-tag".equals(category.getCategory())) {
          linterCategory = category;
        }
      }
    }
  }

  /** Linter category */
  private LinterCategory linterCategory = null;
}