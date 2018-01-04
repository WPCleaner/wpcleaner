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
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTable;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 532 of check wikipedia project.
 * Error 532: Missing end tag (see [[Special:LintErrors/missing-end-tag]])
 */
public class CheckErrorAlgorithm532 extends CheckErrorAlgorithmBase {

  /** Possible global fixes */
  private final static String[] globalFixes = new String[] {
    GT._("Fix tags"),
  };

  public CheckErrorAlgorithm532() {
    super("Missing end tag");
  }

  /** List of tags to be verified. */
  private final static String[] tagNames = {
    PageElementTag.TAG_HTML_B,
    PageElementTag.TAG_HTML_BIG,
    PageElementTag.TAG_HTML_BLOCKQUOTE,
    PageElementTag.TAG_HTML_CENTER,
    PageElementTag.TAG_HTML_CODE,
    PageElementTag.TAG_HTML_DIV,
    PageElementTag.TAG_HTML_EM,
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
    PageElementTag.TAG_HTML_I,
    PageElementTag.TAG_HTML_LI,
    PageElementTag.TAG_HTML_OL,
    PageElementTag.TAG_HTML_P,
    PageElementTag.TAG_HTML_S,
    PageElementTag.TAG_HTML_SMALL,
    PageElementTag.TAG_HTML_SPAN,
    PageElementTag.TAG_HTML_STRIKE,
    PageElementTag.TAG_HTML_STRONG,
    PageElementTag.TAG_HTML_SUB,
    PageElementTag.TAG_HTML_SUP,
    PageElementTag.TAG_HTML_TABLE,
    PageElementTag.TAG_HTML_TD,
    PageElementTag.TAG_HTML_TR,
    PageElementTag.TAG_HTML_TT,
    PageElementTag.TAG_HTML_U,
    PageElementTag.TAG_HTML_UL,
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

    // Analyze each tag
    List<PageElementTag> tags = analysis.getTags();
    boolean result = false;
    for (PageElementTag tag : tags) {
      if (!tag.isComplete() && !tag.isEndTag()) {
        result |= reportIncompleteTag(analysis, tag, errors, onlyAutomatic);
      }
    }

    return result;
  }


  // ==============================================================================================
  // Tag analysis
  // ==============================================================================================

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

    // Tag in table
    if (!hasBeenReported) {
      hasBeenReported = analyzeTable(analysis, tag, errors);
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
      hasBeenReported = analyzeInsideTags(analysis, tag, errors);
    }

    // Headings
    if (!hasBeenReported) {
      hasBeenReported = analyzeHeadings(analysis, tag, errors);
    }

    // Tag in template
    if (!hasBeenReported) {
      hasBeenReported = analyzeTemplate(analysis, tag, errors);
    }

    // Tag in the last line
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
    if (!PageElementTag.TAG_HTML_DIV.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
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
    int beginIndex = tag.getBeginIndex();
    int endIndex = tag.getEndIndex();
    String contents = analysis.getContents();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errorResult.addReplacement(
        contents.substring(beginIndex, endIndex) + PageElementTag.createTag(tag.getName(), true, false),
        true);
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
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag, tag.getBeginIndex(), link.getEndIndex() - 2, true);
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
    if (!PageElementTag.TAG_HTML_BIG.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
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
    int endIndex = image.getBeginIndex() + desc.getEndOffset();
    if ((tag.getBeginIndex() < beginIndex) || (tag.getEndIndex() > endIndex)) {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = analyzeArea(analysis, tag, beginIndex, endIndex, true);
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
  private boolean analyzeGalleryImageDescription(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is in a gallery tag
    PageElementTag galleryTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_GALLERY, tag.getBeginIndex());
    if (galleryTag == null) {
      return false;
    }

    // Retrieve line information
    int beginIndex = tag.getBeginIndex();
    String contents = analysis.getContents();
    while ((beginIndex > 0) &&
           (contents.charAt(beginIndex - 1) != '\n') &&
           (beginIndex > galleryTag.getValueBeginIndex())) {
      beginIndex--;
    }
    while ((beginIndex < tag.getBeginIndex()) &&
           (contents.charAt(beginIndex) != '|')) {
      beginIndex++;
    }
    int endIndex = tag.getEndIndex();
    while ((endIndex < contents.length()) &&
           (contents.charAt(endIndex) != '\n') &&
           (endIndex < galleryTag.getValueEndIndex())) {
      endIndex++;
    }

    // Check that description doesn't span multiple lines
    int tmpIndex = endIndex + 1;
    boolean oneLine = false;
    int colonIndex = contents.indexOf(':', tmpIndex);
    if (colonIndex > tmpIndex) {
      Namespace imageNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);
      if ((imageNamespace != null) &&
          imageNamespace.isPossibleName(contents.substring(tmpIndex, colonIndex))) {
        oneLine = true;
      }
    }
    if (tmpIndex == galleryTag.getValueEndIndex()) {
      oneLine = true;
    }
    if (!oneLine) {
      while ((tmpIndex < contents.length()) &&
             ("[|{<".indexOf(contents.charAt(tmpIndex)) < 0)) {
        tmpIndex++;
      }
      if ((tmpIndex < contents.length()) &&
          (contents.charAt(tmpIndex) == '|')) {
        oneLine = true;
      }
    }

    // Report tag
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag, beginIndex, endIndex, oneLine);
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
  private boolean analyzeTable(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_BIG.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_DIV.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_S.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_TT.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is in a table
    PageElementTable table = analysis.isInTable(tag.getBeginIndex());
    if (table == null) {
      return false;
    }

    // Retrieve cell
    PageElementTable.TableCell cell = table.getCellAtIndex(tag.getBeginIndex());
    if (cell == null) {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag,
        cell.getEndOptionsIndex(), cell.getEndIndex(), true);
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
    if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_DIV.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName())) {
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
              !otherTag.isComplete() &&
              !otherTag.mayBeUnclosed()) {
            automatic = false;
          }
        }
      }
      currentIndex = nextIndex;
    }

    // Report tag
    int beginIndex = param.getValueStartIndex();
    int endIndex = param.getEndIndex();
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag, beginIndex, endIndex, automatic);
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
    if (!PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
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
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag, tag.getBeginIndex(), currentIndex, false); 
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
    CheckErrorResult errorResult = analyzeArea(analysis, tag, lineBegin, lineEnd, true);
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
  private boolean analyzeInsideTags(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check type of tag
    if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName()) &&
        !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
      return false;
    }

    // Analyze if it is inside a tag
    int beginIndex = tag.getBeginIndex() - 1;
    String contents = analysis.getContents();
    while ((beginIndex >= 0) &&
           (" \n".indexOf(contents.charAt(beginIndex)) >= 0)) {
      beginIndex--;
    }
    if ((beginIndex < 0) || (contents.charAt(beginIndex) != '>')) {
      return false;
    }
    PageElementTag surroundingTag = analysis.isInTag(beginIndex);
    if (surroundingTag == null) {
      return false;
    }

    // Analyze if tag combination is valid
    if (PageElementTag.TAG_HTML_CENTER.equals(surroundingTag.getNormalizedName())) {
      if (!PageElementTag.TAG_HTML_FONT.equals(tag.getNormalizedName()) &&
          !PageElementTag.TAG_HTML_SMALL.equals(tag.getNormalizedName()) &&
          !PageElementTag.TAG_HTML_SPAN.equals(tag.getNormalizedName())) {
        return false;
      }
    } else if (PageElementTag.TAG_HTML_DIV.equals(surroundingTag.getNormalizedName())) {
      if (!PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName())) {
        return false;
      }
    } else {
      return false;
    }

    // Report tag
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag,
        surroundingTag.getValueBeginIndex(), surroundingTag.getValueEndIndex(),
        true);
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
    CheckErrorResult errorResult = analyzeArea(
        analysis, tag,
        previousTag.getBeginIndex(), tag.getEndIndex(),
        true);
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

    // TODO: Refactor

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
      CheckErrorResult errorResult = analyzeArea(
          analysis, tag, lastTag.getBeginIndex(), tag.getEndIndex(), !hasOtherTagBefore); 
      if (errorResult != null) {
        errors.add(errorResult);
        return true;
      }
    } else {
      CheckErrorResult errorResult = analyzeArea(
          analysis, tag, tag.getBeginIndex(), lastIndex, !hasOtherTagBefore); 
      if (errorResult != null) {
        errors.add(errorResult);
        return true;
      }
    }
    return false;
  }

  /**
   * Report an error in an area of text.
   * 
   * @param analysis Page analysis.
   * @param tag Tag that could be replaced.
   * @param previousTag Previous opening tag.
   * @param automatic True if automatic modifications can be done.
   * @return Error result if the error can be reported.
   */
  private CheckErrorResult analyzeArea(
      PageAnalysis analysis,
      PageElementTag tag, int beginIndex, int endIndex,
      boolean automatic) {

    // Check parameters
    if ((analysis == null) || (tag == null)) {
      return null;
    }
    int tagBeginIndex = tag.getBeginIndex();
    int tagEndIndex = tag.getEndIndex();
    if ((tagBeginIndex < beginIndex) || (tagEndIndex > endIndex)) {
      return null;
    }

    // Reduce area size if possible
    String contents = analysis.getContents();
    boolean tryReducing = true;
    while (tryReducing) {
      tryReducing = false;

      // Remove whitespace
      while ((beginIndex < tagBeginIndex) &&
             ("\n ".indexOf(contents.charAt(beginIndex)) >= 0)) {
        beginIndex++;
        tryReducing = true;
      }
      while ((endIndex > tagEndIndex) &&
             ("\n ".indexOf(contents.charAt(endIndex - 1)) >= 0)) {
        endIndex--;
        tryReducing = true;
      }

      // Remove comments
      if ((beginIndex < tagBeginIndex) &&
          (contents.charAt(beginIndex) == '<')) {
        PageElementComment comment = analysis.isInComment(beginIndex);
        if ((comment != null) && (comment.getBeginIndex() == beginIndex)) {
          beginIndex = comment.getEndIndex();
          tryReducing = true;
        }
      }
      if ((endIndex > tagEndIndex) &&
          (contents.charAt(endIndex - 1) == '>')) {
        PageElementComment comment = analysis.isInComment(endIndex - 1);
        if ((comment != null) && (comment.getEndIndex() == endIndex)) {
          endIndex = comment.getBeginIndex();
          tryReducing = true;
        }
      }

      // Remove surrounding tags
      if ((beginIndex < tagBeginIndex) &&
          (contents.charAt(beginIndex) == '<') &&
          (endIndex > tagEndIndex) &&
          (contents.charAt(endIndex - 1) == '>')) {
        PageElementTag surroundingTag = analysis.isInTag(beginIndex);
        if ((surroundingTag != null) &&
            (surroundingTag.getCompleteBeginIndex() == beginIndex) &&
            (surroundingTag.getCompleteEndIndex() == endIndex)) {
          beginIndex = surroundingTag.getValueBeginIndex();
          endIndex = surroundingTag.getValueEndIndex();
          tryReducing = true;
        }
      }

      // Remove surrounding bold and italic (if there are none inside)
      if ((beginIndex < tagBeginIndex) &&
          (contents.charAt(beginIndex) == '\'') &&
          (endIndex > tagEndIndex) &&
          (contents.charAt(endIndex - 1) == '\'')) {
        int length = 0;
        while (contents.charAt(beginIndex + length) == '\'') {
          length++;
        }
        boolean ok = true;
        if ((length != 2) && (length != 3) && (length != 5)) {
          ok = false;
        }
        for (int tmpIndex = endIndex - length; tmpIndex < endIndex; tmpIndex++) {
          if (contents.charAt(tmpIndex) != '\'') {
            ok = false;
          }
        }
        for (int tmpIndex = beginIndex + length; tmpIndex < endIndex - length; tmpIndex++) {
          if (contents.startsWith("''", tmpIndex)) {
            ok = false;
          }
        }
        if (ok) {
          beginIndex += length;
          endIndex -= length;
          tryReducing = true;
        }
      }
    }

    // If tag is at the end of the area, check if closing it is possible
    if ((endIndex == tagEndIndex) &&
        !tag.isEndTag() &&
        !tag.isComplete()) {

      // Search for a previous tag
      PageElementTag previousTag = null;
      boolean severalPreviousTag = false;
      boolean otherUnclosedTag = false;
      int tmpIndex = tagBeginIndex;
      while (tmpIndex > beginIndex) {
        tmpIndex--;
        if (contents.charAt(tmpIndex) == '>') {
          PageElementTag tmpTag = analysis.isInTag(tmpIndex);
          if (tmpTag != null) {
            if (!tmpTag.isComplete()) {
              if (tmpTag.getNormalizedName().equals(tag.getNormalizedName())) {
                if (previousTag == null) {
                  previousTag = tmpTag;
                } else {
                  severalPreviousTag = true;
                }
              } else {
                otherUnclosedTag = true;
              }
            }
            tmpIndex = tmpTag.getBeginIndex();
          }
        }
      }

      // If no previous tag, delete or close the tag
      if (previousTag == null) {
        CheckErrorResult errorResult = createCheckErrorResult(analysis, tagBeginIndex, tagEndIndex);
        errorResult.addReplacement(
            "", false);
        errorResult.addReplacement(
            contents.substring(tagBeginIndex, tagEndIndex) + PageElementTag.createTag(tag.getName(), true, false),
            false);
        return errorResult;
      }

      // Check for some situations (not across some other constructions)
      if (!analysis.areInSameArea(previousTag.getEndIndex(), tagBeginIndex)) {
        return null;
      }

      // If previous tag, close the tag
      if (otherUnclosedTag || severalPreviousTag) {
        automatic = false;
      }
      CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, tagEndIndex);
      String replacement =
          contents.substring(beginIndex, tagBeginIndex) +
          PageElementTag.createTag(previousTag.getName(), true, false);
      String text =
          contents.substring(beginIndex, previousTag.getEndIndex()) +
          "..." +
          PageElementTag.createTag(previousTag.getName(), true, false);
      errorResult.addReplacement(replacement, text, automatic);
      return errorResult;
    }

    // Check what is before the tag
    for (int tmpIndex = beginIndex; tmpIndex < tagBeginIndex; tmpIndex++) {
      if (contents.charAt(tmpIndex) == '<') {
        PageElementTag tmpTag = analysis.isInTag(tmpIndex, tag.getNormalizedName());
        if ((tmpTag != null) && !tmpTag.isComplete()) {
          return null;
        }
      }
    }

    // Check for some situations (not across some other constructions)
    if (!analysis.areInSameArea(tagEndIndex, endIndex)) {
      return null;
    }

    // Check what is after the opening tag
    int currentIndex = tag.getEndIndex();
    int countBold = 0;
    int countItalic = 0;
    boolean hasContentsAfter = false;
    while (currentIndex < endIndex) {
      char currentChar = contents.charAt(currentIndex);
      int nextIndex = currentIndex + 1;
      hasContentsAfter |= (" \n".indexOf(currentChar) < 0);
      if (currentChar == '<') {
        PageElementTag currentTag = analysis.isInTag(currentIndex);
        if (currentTag != null) {
          nextIndex = currentTag.getEndIndex();
          if (!currentTag.isComplete() &&
              !currentTag.mayBeUnclosed()) {
            return null;
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
        boolean isSafe = false;
        if (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_TIMELINE, currentIndex) != null) {
          isSafe = true;
        }
        PageElementTable table = analysis.isInTable(currentIndex);
        if (table != null) {
          PageElementTable.TableStart start = table.getTableStart();
          if ((start != null) && (start.containsIndex(currentIndex))) {
            isSafe = true;
          }
        }
        if (!isSafe) {
          automatic = false;
        }
      }
      currentIndex = nextIndex;
    }
    if ((countBold % 2 != 0) || (countItalic % 2 != 0)) {
      automatic = false;
    }
    if (!hasContentsAfter) {
      automatic = false;
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
    if ((countBold % 2 != 0) || (countItalic % 2 != 0)) {
      int contentBegin = tag.getEndIndex();
      int countApostrophe = 0;
      while ((contentBegin < endIndex) &&
             (contents.charAt(contentBegin + countApostrophe) == '\'')) {
        countApostrophe++;
      }
      boolean shouldComplete = false;
      if ((countApostrophe == 5) && (countBold % 2 != 0) && (countItalic % 2 != 0)) {
        shouldComplete = true;
      } else if ((countApostrophe == 3) && (countBold % 2 != 0) && (countItalic % 2 == 0)) {
        shouldComplete = true;
      } else if ((countApostrophe == 2) && (countBold % 2 == 0) && (countItalic % 2 != 0)) {
        shouldComplete = true;
      }
      if (shouldComplete) {
        replacement =
            contents.substring(beginIndex, endIndex) +
            contents.substring(contentBegin, contentBegin + countApostrophe) +
            PageElementTag.createTag(tag.getName(), true, false);
        text =
            contents.substring(beginIndex, tag.getEndIndex()) +
            "..." +
            contents.substring(contentBegin, contentBegin + countApostrophe) +
            PageElementTag.createTag(tag.getName(), true, false);
        errorResult.addReplacement(replacement, text, false);
      }
    }
    if (!hasContentsAfter) {
      errorResult.addReplacement(
          contents.substring(beginIndex, tag.getBeginIndex()) +
          contents.substring(tag.getEndIndex(), endIndex));
    }
    return errorResult;
  }

  // ==============================================================================================
  // General functions
  // ==============================================================================================

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
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
