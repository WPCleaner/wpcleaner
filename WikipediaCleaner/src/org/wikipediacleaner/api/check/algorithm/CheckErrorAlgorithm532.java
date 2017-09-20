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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;


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
    PageElementTag.TAG_HTML_P,
    PageElementTag.TAG_HTML_SMALL,
    PageElementTag.TAG_HTML_SPAN,
    PageElementTag.TAG_HTML_TD,
    PageElementTag.TAG_HTML_TR,
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
    boolean result = false;
    for (String tagName : tagNames) {
      result |= analyzeTag(analysis, tagName, errors, onlyAutomatic);
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param tagName Tag name.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis, String tagName,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    boolean errorFound = false;
    List<PageElementTag> tags = analysis.getTags(tagName);
    for (PageElementTag tag : tags) {
      if (!tag.isComplete() && !tag.isEndTag()) {
        errorFound = true;
        if (errors != null) {
          reportTag(analysis, tag, errors, onlyAutomatic);
        }
      }
    }
    return errorFound;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tag.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   */
  private void reportTag(
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

    // Tag in list item
    if (!hasBeenReported) {
      hasBeenReported = analyzeListItem(analysis, tag, errors);
    }

    // Tag inside center tags
    if (!hasBeenReported) {
      hasBeenReported = analyzeInsideCenterTags(analysis, tag, errors);
    }

    // Default reporting
    if (!hasBeenReported) {
      CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), tag.getEndIndex());
      errors.add(errorResult);
    }
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
    String contents = analysis.getContents();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), tag.getEndIndex());
    String replacement =
        contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
        PageElementTag.createTag(tag.getName(), true, false);
    errorResult.addReplacement(
        replacement, true);
    errors.add(errorResult);
    return true;
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
    if (!PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
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
    int endIndex = link.getEndIndex() - 2;
    CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), endIndex);
    String replacement =
        contents.substring(tag.getBeginIndex(), endIndex) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
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
    if ((desc == null) || (tag.getBeginIndex() != image.getBeginIndex() + desc.getBeginOffset())) {
      return false;
    }

    // Check description
    int currentIndex = tag.getEndIndex();
    String contents = analysis.getContents();
    while (currentIndex < image.getBeginIndex() + desc.getEndOffset()) {
      char currentChar = contents.charAt(currentIndex);
      if (currentChar == '<') {
        PageElementTag secondTag = analysis.isInTag(currentIndex, tag.getNormalizedName());
        if (secondTag != null) {
          return false;
          // TODO
        }
      }
      currentIndex++;
    }

    // Report tag
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        image.getBeginIndex() + desc.getBeginOffset(),
        image.getBeginIndex() + desc.getEndOffset());
    String replacement =
        contents.substring(image.getBeginIndex() + desc.getBeginOffset(), image.getBeginIndex() + desc.getEndOffset()) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
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
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, tag.getBeginIndex(), secondTag.getEndIndex());
              String replacement =
                  contents.substring(tag.getBeginIndex(), secondTag.getBeginIndex()) +
                  PageElementTag.createTag(tag.getName(), true, false);
              String text =
                  contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
                  "..." +
                  PageElementTag.createTag(tag.getName(), true, false);
              errorResult.addReplacement(
                  replacement, text, true);
              errors.add(errorResult);
              return true;
            }
            return false;
          }
        }
        currentIndex++;
      }
    }

    // Report tag
    CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), currentIndex);
    String replacement =
        contents.substring(tag.getBeginIndex(), currentIndex) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
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
    CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), currentIndex);
    String replacement =
        contents.substring(tag.getBeginIndex(), currentIndex) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
        "..." +
        PageElementTag.createTag(tag.getName(), true, false);
    errorResult.addReplacement(
        replacement, text, false);
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
    CheckErrorResult errorResult = createCheckErrorResult(analysis, tag.getBeginIndex(), currentIndex);
    String replacement =
        contents.substring(tag.getBeginIndex(), currentIndex) +
        PageElementTag.createTag(tag.getName(), true, false);
    String text =
        contents.substring(tag.getBeginIndex(), tag.getEndIndex()) +
        "..." +
        PageElementTag.createTag(tag.getName(), true, false);
    errorResult.addReplacement(
        replacement, text, true);
    errors.add(errorResult);
    return true;
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
