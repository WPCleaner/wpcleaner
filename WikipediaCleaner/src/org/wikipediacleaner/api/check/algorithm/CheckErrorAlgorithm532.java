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

    // Tags in image description
    if (!hasBeenReported) {
      hasBeenReported = analyzeImageDescription(analysis, tag, errors);
    }

    // Tags in image description in gallery tags
    if (!hasBeenReported) {
      hasBeenReported = analyzeGalleryImageDescription(analysis, tag, errors);
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
