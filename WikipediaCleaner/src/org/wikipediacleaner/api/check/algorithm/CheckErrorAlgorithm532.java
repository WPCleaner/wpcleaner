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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 532 of check wikipedia project.
 * Error 532: Missing end tag (see [[Special:LintErrors/missing-end-tag]])
 */
public class CheckErrorAlgorithm532 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm532() {
    super("Missing end tag");
  }

  /** List of tags to be verified. */
  private final static String[] tagNames = {
    PageElementTag.TAG_HTML_CENTER,
    PageElementTag.TAG_HTML_DIV,
    PageElementTag.TAG_HTML_FONT,
    PageElementTag.TAG_HTML_H1,
    PageElementTag.TAG_HTML_H2,
    PageElementTag.TAG_HTML_H3,
    PageElementTag.TAG_HTML_H4,
    PageElementTag.TAG_HTML_H5,
    PageElementTag.TAG_HTML_H6,
    PageElementTag.TAG_HTML_H7,
    PageElementTag.TAG_HTML_H8,
    PageElementTag.TAG_HTML_H9,
    PageElementTag.TAG_HTML_LI,
    PageElementTag.TAG_HTML_P,
    PageElementTag.TAG_HTML_S,
    PageElementTag.TAG_HTML_SMALL,
    PageElementTag.TAG_HTML_SPAN,
    PageElementTag.TAG_HTML_STRIKE,
    PageElementTag.TAG_HTML_TD,
    PageElementTag.TAG_HTML_TR,
  };

  /**
   * Tags that are OK to be unclosed.
   */
  private final static String[] unclosedTags = {
    PageElementTag.TAG_HTML_BR
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

    // Analyze each reference tag
    List<PageElementTag> tags = analysis.getTags();
    boolean result = false;
    for (PageElementTag tag : tags) {
      if (!tag.isComplete() && !tag.isEndTag()) {
        if (errors != null) {
          result |= reportIncompleteTag(analysis, tag, errors, onlyAutomatic);
        }
      }
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean reportIncompleteTag(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    boolean hasBeenReported = false;

    // Id tag
    if (!hasBeenReported) {
      hasBeenReported = analyzeIdTag(analysis, tag, errors);
    }

    // Tag in link text
    if (!hasBeenReported) {
      hasBeenReported = analyzeLinkText(analysis, tag, errors);
    }

    // Tag in image description
    if (!hasBeenReported) {
      hasBeenReported = analyzeImageDescription(analysis, tag, errors);
    }

    // Tag in image description in gallery tags
    if (!hasBeenReported) {
      hasBeenReported = analyzeGalleryImageDescription(analysis, tag, errors);
    }

    // Tag in template
    if (!hasBeenReported) {
      hasBeenReported = analyzeTemplate(analysis, tag, errors);
    }

    // Tag in list item
    if (!hasBeenReported) {
      hasBeenReported = analyzeListItem(analysis, tag, errors);
    }

    // Tag for HTML list
    if (!hasBeenReported) {
      hasBeenReported = analyzeHTMLList(analysis, tag, errors);
    }

    // Tag inside center tags
    if (!hasBeenReported) {
      hasBeenReported = analyzeInsideCenterTags(analysis, tag, errors);
    }

    // Headings
    if (!hasBeenReported) {
      hasBeenReported = analyzeHeadings(analysis, tag, errors);
    }

    // Tag in the next line
    if (!hasBeenReported) {
      hasBeenReported = analyzeLastLine(analysis, tag, errors);
    }

    // Default reporting
    if (!hasBeenReported) {
      boolean shouldBeReported = false;
      for (String tagName : tagNames) {
        if (tagName.equals(tag.getNormalizedName())) {
          shouldBeReported = true;
        }
      }
      if (shouldBeReported) {
        CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), tag.getEndIndex());
        errors.add(errorResult);
        hasBeenReported = true;
      }
    }

    return hasBeenReported;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeIdTag(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is an id tag
    boolean idAttribute = false;
    boolean otherAttribute = false;
    for (int paramNum = 0; paramNum < tag.getParametersCount(); paramNum++) {
      PageElementTag.Parameter param = tag.getParameter(paramNum);
      if ("id".equalsIgnoreCase(param.getName())) {
        idAttribute = true;
      } else {
        otherAttribute = true;
      }
    }
    if (!idAttribute || otherAttribute) {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = closeTag(analysis, tag, tag.getBeginIndex(), tag.getEndIndex(), true);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeLinkText(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is inside link text
    PageElementInternalLink link = analysis.isInInternalLink(tag.getBeginIndex());
    if ((link == null) ||
        (link.getText() == null) ||
        (tag.getBeginIndex() < link.getBeginIndex() + link.getTextOffset())) {
      return false;
    }
    String contents = analysis.getContents();
    int index = tag.getBeginIndex();
    while ((index > 0) && (contents.charAt(index - 1) == ' ')) {
      index--;
    }
    if (index > link.getBeginIndex() + link.getTextOffset()) {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult =
        closeTag(analysis, tag, tag.getBeginIndex(), link.getEndIndex() - 2, true);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeImageDescription(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is in an image description
    PageElementImage image = analysis.isInImage(tag.getBeginIndex());
    if (image == null) {
      return false;
    }
    Parameter desc = image.getDescriptionParameter();
    if (desc == null) {
      return false;
    }
    int beginIndex = image.getBeginIndex() + desc.getBeginOffset();

    // Report tag
    int endIndex = image.getBeginIndex() + desc.getEndOffset();
    if (tag.getBeginIndex() == beginIndex) {

      // Tag at the beginning of the description
      CheckErrorResult errorResult = closeTag(analysis, tag, beginIndex, endIndex, true);
      if (errorResult != null) {
        errors.add(errorResult);
        return true;
      }
    } else {

      // Check for tag at the end of the description
      String contents = analysis.getContents();
      while ((endIndex > 0) && (contents.charAt(endIndex - 1) == ' ')) {
        endIndex--;
      }
      if (tag.getEndIndex() == endIndex) {
        while (contents.charAt(beginIndex) == ' ') {
          beginIndex++;
        }
        if (contents.charAt(beginIndex) == '<') {
          PageElementTag previousTag = analysis.isInTag(beginIndex, tag.getNormalizedName());
          if (previousTag != null) {
            CheckErrorResult errorResult = replaceByClosingTag(analysis, tag, previousTag, true);
            if (errorResult != null) {
              errors.add(errorResult);
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeGalleryImageDescription(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is in a gallery tag
    PageElementTag galleryTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_GALLERY, tag.getBeginIndex());
    if (galleryTag == null) {
      return false;
    }

    // Check description
    int currentIndex = tag.getBeginIndex();
    String contents = analysis.getContents();
    while ((currentIndex > 0) && (contents.charAt(currentIndex - 1) == ' ')) {
      currentIndex--;
    }
    if ((currentIndex <= 0) || (contents.charAt(currentIndex - 1) != '|')) {
      return false;
    }
    currentIndex = tag.getEndIndex();
    boolean finished = false;
    while (!finished && (currentIndex < contents.length())) {
      char currentChar = contents.charAt(currentIndex);
      if (currentChar == '\n') {
        finished = true;
      } else {
        if (currentChar == '<') {
          PageElementTag secondTag = analysis.isInTag(currentIndex, tag.getNormalizedName());
          if (secondTag != null) {
            if (secondTag.isComplete() || secondTag.isEndTag()) {
              return false;
            }
            currentIndex = secondTag.getEndIndex();
            while ((currentIndex < contents.length()) && (contents.charAt(currentIndex) == ' ')) {
              currentIndex++;
            }
            if ((currentIndex < contents.length()) && (contents.charAt(currentIndex) == '\n')) {
              finished = true;
              CheckErrorResult errorResult = replaceByClosingTag(analysis, secondTag, tag, true);
              if (errorResult != null) {
                errors.add(errorResult);
                return true;
              }
            }
            return false;
          }
        }
        currentIndex++;
      }
    }

    // Report tag
    CheckErrorResult errorResult = closeTag(analysis, tag, tag.getBeginIndex(), currentIndex, true);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is in a template argument
    PageElementTemplate template = analysis.isInTemplate(tag.getBeginIndex());
    if (template == null) {
      return false;
    }
    PageElementTemplate.Parameter param = template.getParameterAtIndex(tag.getBeginIndex());
    if (param == null) {
      return false;
    }
    boolean automatic = true;

    // Check if it's the only unclosed tag in the template
    int currentIndex = template.getBeginIndex();
    String contents = analysis.getContents();
    while (automatic && (currentIndex < template.getEndIndex())) {
      char currentChar = contents.charAt(currentIndex);
      int nextIndex = currentIndex + 1;
      if (currentChar == '<'){
        PageElementTag otherTag = analysis.isInTag(currentIndex);
        if (otherTag != null) {
          nextIndex = otherTag.getEndIndex();
          if ((currentIndex != tag.getBeginIndex()) &&
              (!otherTag.isComplete())) {
            automatic = false;
          }
        }
      }
      currentIndex = nextIndex;
    }

    // Limits
    int beginIndex = param.getValueStartIndex();
    int endIndex = param.getEndIndex();
    while ((endIndex > 0) &&
           (" \n".indexOf(contents.charAt(endIndex - 1)) >= 0)) {
      endIndex--;
    }

    // Report tag
    CheckErrorResult errorResult = closeTag(analysis, tag, beginIndex, endIndex, automatic);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeListItem(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is in a list item
    String contents = analysis.getContents();
    int index = tag.getBeginIndex();
    while ((index > 0) && (contents.charAt(index - 1) != '\n')) {
      index--;
    }
    if (contents.charAt(index) != '*') {
      return false;
    }

    // Check item
    int currentIndex = tag.getEndIndex();
    boolean textFound = false;
    while ((currentIndex < contents.length()) &&
        (contents.charAt(currentIndex) != '\n')) {
      textFound |= !Character.isWhitespace(contents.charAt(currentIndex));
      currentIndex++;
    }
    if (!textFound) {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = closeTag(analysis, tag, tag.getBeginIndex(), currentIndex, false);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeHTMLList(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_LI.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is inside a HTML list
    PageElementTag tagList = null;
    PageElementTag tagOL = analysis.getSurroundingTag(PageElementTag.TAG_HTML_OL, tag.getBeginIndex());
    if (tagOL != null) {
      tagList = tagOL;
    }
    PageElementTag tagUL = analysis.getSurroundingTag(PageElementTag.TAG_HTML_UL, tag.getBeginIndex());
    if (tagUL != null) {
      if (tagList == null) {
        tagList = tagUL;
      } else if (tagUL.getBeginIndex() > tagList.getBeginIndex()) {
        tagList = tagUL;
      }
    }
    if (tagList == null) {
      return false;
    }

    // Analyze the line
    String contents = analysis.getContents();
    int lineBegin = tag.getBeginIndex();
    if ((lineBegin <= 1) || (contents.charAt(lineBegin - 1) != '\n')) {
      return false;
    }
    int lineEnd = tag.getEndIndex();
    while ((lineEnd < contents.length()) &&
           (contents.charAt(lineEnd) != '\n') &&
           (contents.charAt(lineEnd) != '<')) {
      lineEnd++;
    }
    if ((lineEnd + 1 >= contents.length()) ||
        (contents.charAt(lineEnd) != '\n')) {
      return false;
    }

    // Analyze the next line
    PageElementTag nextLineTag = analysis.isInTag(lineEnd + 1);
    if (nextLineTag == null) {
      return false;
    }
    if (PageElementTag.TAG_HTML_LI.equals(nextLineTag.getNormalizedName())) {
      if (nextLineTag.isEndTag() || nextLineTag.isFullTag()) {
        return false;
      }
    } else if (tagList.getNormalizedName().equals(nextLineTag.getNormalizedName())) {
      if (!nextLineTag.isEndTag() || nextLineTag.isFullTag()) {
        return false;
      }
    } else {
      return false;
    }

    // Analyze the previous line
    int tmpIndex = lineBegin - 1;
    while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) != '\n')) {
      tmpIndex--;
    }
    PageElementTag previousLineTag = analysis.isInTag(tmpIndex);
    if (previousLineTag == null){
      return false;
    }
    if (PageElementTag.TAG_HTML_LI.equals(previousLineTag.getNormalizedName())) {
      if (previousLineTag.isEndTag() || previousLineTag.isFullTag()) {
        return false;
      }
    } else if (previousLineTag == tagList) {
      if (tagList.getEndIndex() < lineBegin - 1) {
        return false;
      }
    } else {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = createCheckErrorResult(analysis, lineBegin, lineEnd);
    String replacement =
        contents.substring(lineBegin, lineEnd) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(lineBegin, tag.getEndIndex()) +
        "..." +
        PageElementTag.createTag(tag.getName(), true, false);
    errorResult.addReplacement(
        replacement, text, true);
    errors.add(errorResult);
    return true;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeInsideCenterTags(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is inside center tags
    String contents = analysis.getContents();
    int index = tag.getBeginIndex();
    while ((index > 0) && (contents.charAt(index - 1) == ' ')) {
      index--;
    }
    if ((index <= 0) || (contents.charAt(index - 1) != '>')) {
      return false;
    }
    PageElementTag centerTag = analysis.isInTag(index - 1, PageElementTag.TAG_HTML_CENTER);
    if ((centerTag == null) || !centerTag.isComplete() || centerTag.isFullTag()){
      return false;
    }

    // Check item
    int currentIndex = tag.getEndIndex();
    while (currentIndex < centerTag.getValueEndIndex()) {
      if (contents.charAt(currentIndex) == '\n') {
        return false;
      }
      currentIndex++;
    }

    // Report tag
    CheckErrorResult errorResult = closeTag(analysis, tag, tag.getBeginIndex(), currentIndex, true);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeHeadings(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_H1.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H2.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H3.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H4.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H5.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H6.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H7.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H8.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_H9.equals(tag.getNormalizedName())) {
      return false;
    }

    // Check that the tag is at the end of a line
    String contents = analysis.getContents();
    int endIndex = tag.getEndIndex();
    while ((endIndex < contents.length()) &&
        (contents.charAt(endIndex) == ' ')) {
      endIndex++;
    }
    if ((endIndex < contents.length()) &&
        (contents.charAt(endIndex) != '\n')) {
      return false;
    }

    // Search previous tag
    PageElementTag previousTag = null;
    int index = tag.getBeginIndex();
    while ((index > 0) && (previousTag == null)) {
      char previousChar = contents.charAt(index - 1);
      if (previousChar == '\n') {
        return false;
      }
      if (previousChar == '>') {
        previousTag = analysis.isInTag(index - 1, tag.getNormalizedName());
      }
      index--;
    }
    if ((previousTag == null) ||
        (!tag.getNormalizedName().equals(previousTag.getNormalizedName()))) {
      return false;
    }

    // Check that the previous tag is at the beginning of a line
    if ((previousTag.getBeginIndex() > 0) &&
        (contents.charAt(previousTag.getBeginIndex() - 1) != '\n')) {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = replaceByClosingTag(analysis, tag, previousTag, true);
    if (errorResult != null) {
      errors.add(errorResult);
      return true;
    }
    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @return True if the error has been reported.
   */
  private boolean analyzeLastLine(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName())) {
      return false;
    }

    // Check namespace
    Integer namespace = analysis.getPage().getNamespace();
    if ((namespace == null) || (namespace.intValue() == Namespace.TEMPLATE)) {
      return false;
    }

    // Go to the end of the last line
    String contents = analysis.getContents();
    int index = contents.length();
    while ((index > 0) && (contents.charAt(index - 1) == '\n')) {
      index--;
    }
    while ((index > 0) && (contents.charAt(index - 1) == ' ')) {
      index--;
    }
    if (index == 0) {
      return false;
    }

    // Check if there's a tag at the end of the last line
    int lastIndex = index;
    PageElementTag lastTag = null;
    if (contents.charAt(index - 1) == '>') {
      lastTag = analysis.isInTag(index - 1);
      if (lastTag != null) {
        index = lastTag.getBeginIndex();
      }
    }

    // Check if the tag is in the last line
    int tagEndIndex = tag.getEndIndex();
    while ((index > tagEndIndex) && (contents.charAt(index - 1) != '\n')) {
      index--;
      if (contents.charAt(index) == '>') {
        if (analysis.isInTag(index - 1, tag.getNormalizedName()) != null) {
          return false;
        }
      }
    }
    if (index > tagEndIndex) {
      return false;
    }

    // Check if there's an other opening tag before in the page
    List<PageElementTag> tags = analysis.getTags(tag.getNormalizedName());
    boolean after = false;
    boolean hasOtherTagBefore = false;
    for (PageElementTag otherTag : tags) {
      if (!after) {
        if (otherTag == tag) {
          after = true;
        } else if (!otherTag.isComplete()) {
          hasOtherTagBefore = true;
        }
      }
    }

    // Decide what to do
    if (lastTag == tag) {
      CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), tag.getEndIndex());
      errorResult.addReplacement("");
      if (hasOtherTagBefore && (tag.getParametersCount() == 0)) {
        errorResult.addReplacement(PageElementTag.createTag(tag.getName(), true, false));
      }
      errors.add(errorResult);
      return true;
    }
    if (lastTag != null) {
      CheckErrorResult errorResult = replaceByClosingTag(analysis, lastTag, tag, !hasOtherTagBefore);
      if (errorResult != null) {
        errors.add(errorResult);
        return true;
      }
    } else {
      CheckErrorResult errorResult = closeTag(analysis, tag, tag.getBeginIndex(), lastIndex, !hasOtherTagBefore);
      if (errorResult != null) {
        errors.add(errorResult);
        return true;
      }
    }
    return false;
  }

  // ==============================================================================================
  // Elementary functions to report an error and propose a given fix.
  // ==============================================================================================

  /**
   * Report an error that can be fixed by closing the opening tag later in the text.
   * 
   * @param analysis Page analysis.
   * @param tag Tag that could be replaced.
   * @param previousTag Previous opening tag.
   * @param automatic True if automatic modifications can be done.
   * @return Error result if the error can be reported.
   */
  private CheckErrorResult closeTag(
      PageAnalysis analysis,
      PageElementTag tag, int beginIndex, int endIndex,
      boolean automatic) {

    // Check tag
    if ((tag == null) || tag.isEndTag() || tag.isComplete()) {
      return null;
    }
    if ((tag.getBeginIndex() < beginIndex) || (tag.getEndIndex() > endIndex)) {
      return null;
    }

    // Check what is after the opening tag
    int currentIndex = tag.getEndIndex();
    String contents = analysis.getContents();
    int countBold = 0;
    int countItalic = 0;
    boolean hasContentsAfter = false;
    while (currentIndex < endIndex) {
      char currentChar = contents.charAt(currentIndex);
      int nextIndex = currentIndex + 1;
      hasContentsAfter |= (" \n".indexOf(currentChar) >= 0);
      if (currentChar == '<') {
        PageElementTag currentTag = analysis.isInTag(currentIndex);
        if (currentTag != null) {
          nextIndex = currentTag.getEndIndex();
          if ((currentTag.getCompleteBeginIndex() < tag.getBeginIndex()) ||
              (currentTag.getCompleteEndIndex() > endIndex)) {
            return null;
          }
          if (!currentTag.isComplete()) {
            boolean normal = false;
            for (String tagName : unclosedTags) {
              if (tagName.equals(currentTag.getNormalizedName())) {
                normal = true;
              }
            }
            if (!normal) {
              return null;
            }
          }
        }
      } else if (currentChar == '\'') {
        while ((nextIndex < contents.length()) && (contents.charAt(nextIndex) == '\'')) {
          nextIndex++;
        }
        switch (nextIndex - currentIndex) {
        case 1:
          break;
        case 2:
          countItalic++;
          break;
        case 3:
          countBold++;
          break;
        case 5:
          countItalic++;
          countBold++;
          break;
        default:
          automatic = false;
        }
      } else if (contents.startsWith(tag.getName(), currentIndex)) {
        automatic = false;
      }
      currentIndex = nextIndex;
    }
    if ((countBold % 2 != 0) || (countItalic % 2 != 0)) {
      automatic = false;
    }
    if (!hasContentsAfter) {
      automatic = false;
    }

    // Check for some situations (not across some other constructions)
    PageElementInternalLink iLink = analysis.isInInternalLink(tag.getBeginIndex());
    if (iLink != null) {
      if (iLink.getEndIndex() < endIndex) {
        return null;
      }
    } else {
      if (analysis.isInInternalLink(endIndex) != null) {
        return null;
      }
    }
    PageElementExternalLink eLink = analysis.isInExternalLink(tag.getBeginIndex());
    if (eLink != null) {
      if (eLink.getEndIndex() < endIndex) {
        return null;
      }
    } else {
      if (analysis.isInExternalLink(endIndex) != null) {
        return null;
      }
    }
    PageElementTemplate template = analysis.isInTemplate(tag.getBeginIndex());
    if (template != null) {
      if (template.getEndIndex() < endIndex) {
        return null;
      }
      PageElementTemplate.Parameter param = template.getParameterAtIndex(tag.getBeginIndex());
      if ((param != null) && (param.getEndIndex() < endIndex)) {
        return null;
      }
    } else {
      if (analysis.isInTemplate(endIndex) != null) {
        return null;
      }
    }

    // Create error
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    String replacement =
        contents.substring(beginIndex, endIndex) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(beginIndex, tag.getEndIndex()) +
        "..." +
            PageElementTag.createTag(tag.getName(), true, false);
    errorResult.addReplacement(replacement, text, automatic);
    if (!hasContentsAfter) {
      errorResult.addReplacement(
          contents.substring(beginIndex, tag.getBeginIndex()) +
          contents.substring(tag.getEndIndex(), endIndex));
    }
    return errorResult;
  }

  /**
   * Report an error that can be fixed by replacing the opening tag by a closing one.
   * 
   * @param analysis Page analysis.
   * @param tag Tag that could be replaced.
   * @param previousTag Previous opening tag.
   * @param automatic True if automatic modifications can be done.
   * @return Error result if the error can be reported.
   */
  private CheckErrorResult replaceByClosingTag(
      PageAnalysis analysis,
      PageElementTag tag, PageElementTag previousTag,
      boolean automatic) {

    // Check tags
    if ((tag == null) || (previousTag == null)) {
      return null;
    }
    if (tag.isEndTag() || previousTag.isEndTag()) {
      return null;
    }
    if (tag.isComplete() || previousTag.isComplete()) {
      return null;
    }
    if (!tag.getNormalizedName().equals(previousTag.getNormalizedName())) {
      return null;
    }
    if (tag.getParametersCount() > 0) {
      return null;
    }

    // TODO: extra verifications ?

    // Create error
    CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), tag.getEndIndex());
    errorResult.addReplacement(PageElementTag.createTag(previousTag.getName(), true, false), automatic);
    return errorResult;
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
