/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


/**
 * Bean for memorizing formatting elements
 */
public class PageElementFormatting {

  /** Page analysis */
  private final PageAnalysis analysis;

  /** Index of the formatting element in the text */
  private final int index;

  /** Length of the formatting element */
  private final int length;

  /** True when element has been analyzed */
  private boolean analyzed;

  /** Beginning of the main area in which the element is */
  private int mainAreaBegin;

  /** End of the main area in which the element is */
  private int mainAreaEnd;

  /** Reference tag in which the element is */
  private PageElementTag inRefTag;

  /** Internal link in which the element is */
  private PageElementInternalLink inILink;

  /** External link in which the element is */
  private PageElementExternalLink inELink;

  /** Template in which the element is */
  private PageElementTemplate inTemplate;

  /** Template parameter in which the element is */
  private PageElementTemplate.Parameter inTemplateParameter;

  /** Title in which the element is */
  private PageElementTitle inTitle;

  /** Image in which the element is */
  private PageElementImage inImage;

  /** List item in which the element is */
  private PageElementListItem inListItem;

  /** Table in which the element is */
  private PageElementTable inTable;

  /** Table caption in which the element is */
  private PageElementTable.TableCaption inTableCaption;

  /** Table cell in which the element is */
  private PageElementTable.TableCell inTableCell;

  /**
   * @param index Begin index of the formatting element.
   * @param length Length of the formatting element.
   */
  private PageElementFormatting(
      PageAnalysis analysis, int index, int length) {
    this.analysis = analysis;
    this.index = index;
    this.length = length;
    this.analyzed = false;
  }

  /**
   * @return Index of the formatting element in the text.
   */
  public int getIndex() {
    return index;
  }

  /**
   * @return Length of the formatting element.
   */
  public int getLength() {
    return length;
  }

  /**
   * @return Meaningful length.
   */
  public int getMeaningfulLength() {
    switch (length) {
    case 2:
    case 3:
    case 5:
      return length;
    case 4:
      return 3;
    default:
      return 5;
    }
  }

  /**
   * @return True if formatting element has bold.
   */
  public boolean isBold() {
    return (length > 2);
  }

  /**
   * @return True if formatting element has italic.
   */
  public boolean isItalic() {
    return ((length == 2) || (length >= 5));
  }

  /**
   * Perform an analysis of the element.
   */
  private void analyze() {
    if (analyzed) {
      return;
    }

    // Analyze element compared to each kind of other elements
    inRefTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, index);
    inILink = analysis.isInInternalLink(index);
    inELink = analysis.isInExternalLink(index);
    inTemplate = analysis.isInTemplate(index);
    if (inTemplate != null) {
      inTemplateParameter = inTemplate.getParameterAtIndex(index);
    } else {
      inTemplateParameter = null;
    }
    inTitle = analysis.isInTitle(index);
    inImage = analysis.isInImage(index);
    inListItem = analysis.isInListItem(index);
    inTable = analysis.isInTable(index);
    if (inTable != null) {
      PageElementTable.TableCaption caption = inTable.getTableCaption();
      if ((caption != null) && (caption.containsIndex(index))) {
        inTableCaption = caption;
      } else {
        inTableCaption = null;
      }
      inTableCell = inTable.getCellAtIndex(index);
    } else {
      inTableCaption = null;
      inTableCell = null;
    }
    // TODO: add paragraph

    // Compute main area
    computeMainArea();

    analyzed = true;
  }

  /**
   * @return Beginning of the main area in which the element is.
   */
  public int getMainAreaBegin() {
    return mainAreaBegin;
  }

  /**
   * @return End of the main area in which the element is.
   */
  public int getMainAreaEnd() {
    return mainAreaEnd;
  }

  /**
   * @return Reference tag in which the element is.
   */
  public PageElementTag isInRefTag() {
    analyze();
    return inRefTag;
  }

  /**
   * @return Internal link in which the element is.
   */
  public PageElementInternalLink isInInternalLink() {
    analyze();
    return inILink;
  }

  /**
   * @return External link in which the element is.
   */
  public PageElementExternalLink isInExternalLink() {
    analyze();
    return inELink;
  }

  /**
   * @return Title in which the element is.
   */
  public PageElementTitle isInTitle() {
    analyze();
    return inTitle;
  }

  /**
   * @return Image in which the element is.
   */
  public PageElementImage isInImage() {
    analyze();
    return inImage;
  }

  /**
   * @return List item in which the element is.
   */
  public PageElementListItem isInListItem() {
    analyze();
    return inListItem;
  }

  /**
   * @return Table caption in which the element is.
   */
  public PageElementTable.TableCaption isInTableCaption() {
    analyze();
    return inTableCaption;
  }

  /**
   * @return Table cell in which the element is.
   */
  public PageElementTable.TableCell isInTableCell() {
    analyze();
    return inTableCell;
  }

  /**
   * @return Template parameter in which the element is.
   */
  public PageElementTemplate.Parameter isInTemplateParameter() {
    analyze();
    return inTemplateParameter;
  }

  /**
   * @return Main area in which the formatting element is.
   */
  private void computeMainArea() {
    mainAreaBegin = 0;
    mainAreaEnd = analysis.getContents().length();

    // Check inside a reference tag
    if (inRefTag != null) {
      mainAreaBegin = inRefTag.getValueBeginIndex();
      mainAreaEnd = inRefTag.getValueEndIndex();
    }

    // Check inside a title
    if (inTitle != null) {
      mainAreaBegin = Math.max(mainAreaBegin, inTitle.getBeginIndex());
      mainAreaEnd = Math.min(mainAreaEnd, inTitle.getEndIndex());
    }

    // Check inside an image
    // NOTE: Unsafe for cases like [[File:...|''...]]''
    //if (inImage != null) {
    //  mainAreaBegin = Math.max(mainAreaBegin, inImage.getBeginIndex());
    //  mainAreaEnd = Math.min(mainAreaEnd, inImage.getEndIndex());
    //}

    // Check inside a table caption
    if ((inTable != null) && (inTableCaption != null)) {
      mainAreaBegin = Math.max(mainAreaBegin, inTableCaption.getBeginIndex());
      mainAreaEnd = Math.min(mainAreaEnd, inTableCaption.getEndIndex());
    }

    // Check inside a table cell
    if ((inTable != null) && (inTableCell != null)) {
      mainAreaBegin = Math.max(mainAreaBegin, inTableCell.getBeginIndex());
      mainAreaEnd = Math.min(mainAreaEnd, inTableCell.getEndIndex());
    }

    // Check inside a list item
    if (inListItem != null) {
      mainAreaBegin = Math.max(mainAreaBegin, inListItem.getBeginIndex());
      mainAreaEnd = Math.min(mainAreaEnd, inListItem.getEndIndex());
    }

    // TODO: Add paragraph
  }

  /**
   * @param elements Elements.
   * @return True if an other element is in the same are.
   */
  public boolean isAloneInArea(List<PageElementFormatting> elements) {

    // If no elements, obviously alone
    if (elements == null) {
      return true;
    }

    // If main area is unknown, be safe and assume not alone
    if ((mainAreaBegin == 0) &&
        (mainAreaEnd == analysis.getContents().length())) {
      return false;
    }

    // Check that other elements are not in the same main area
    for (PageElementFormatting element : elements) {
      if (element != this) {
        if ((element.index >= mainAreaBegin) &&
            (element.index < mainAreaEnd)) {
          return false;
        }
        if ((index >= element.mainAreaBegin) &&
            (index < element.mainAreaEnd)) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * @param closeIndex Index where to close the formatting element.
   * @return True if the formatting element can be closed safely.
   */
  public boolean canBeClosedAt(int closeIndex) {

    // Check that the close index is in the same areas than the element
    if ((inRefTag != null) && (!inRefTag.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inILink != null) && (!inILink.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inELink != null) && (!inELink.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inTemplate != null) && (!inTemplate.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inTemplateParameter != null) && (!inTemplateParameter.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inTitle != null) && (!inTitle.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inImage != null) && (!inImage.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inListItem != null) && (!inListItem.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inTable != null) && (!inTable.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inTableCaption != null) && (!inTableCaption.containsIndex(closeIndex - 1))) {
      return false;
    }
    if ((inTableCell != null) && (!inTableCell.containsIndex(closeIndex - 1))) {
      return false;
    }

    // Check that there are no line breaks
    String contents = analysis.getContents();
    for (int tmpIndex = index + length; tmpIndex < closeIndex; tmpIndex++) {
      if (contents.charAt(tmpIndex) == '\n') {
        return false;
      }
    }

    // TODO: Check that closeIndex is not inside something else ?

    return true;
  }

  /**
   * @param analysis Page analysis.
   * @return List of formatting elements in the page.
   */
  public static List<PageElementFormatting> listFormattingElements(
      PageAnalysis analysis) {

    // Analyze contents for formatting elements
    List<PageElementFormatting> elements = new ArrayList<>();
    String contents = analysis.getContents();
    int index = 0;
    do {
      index = contents.indexOf('\'', index);
      if (index >= 0) {
        int length = 1;
        while ((index + length < contents.length()) &&
               (contents.charAt(index + length) == '\'')) {
          length++;
        }
        if (length > 1) {
          elements.add(new PageElementFormatting(analysis, index, length));
        }
        index += length;
      }
    } while (index >= 0);

    // Exclude comments
    List<PageElementComment> comments = analysis.getComments();
    for (PageElementComment comment : comments) {
      PageElementFormatting.excludeArea(
          elements, comment.getBeginIndex(), comment.getEndIndex());
    }

    // Exclude some tags
    String[] tagsExclusions = new String[] {
        PageElementTag.TAG_WIKI_CHEM,
        PageElementTag.TAG_WIKI_MATH,
        PageElementTag.TAG_WIKI_MATH_CHEM,
        PageElementTag.TAG_WIKI_NOWIKI,
        PageElementTag.TAG_WIKI_SOURCE,
        PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT,
        PageElementTag.TAG_WIKI_TIMELINE,
    };
    for (String tagsExclusion : tagsExclusions) {
      List<PageElementTag> tags = analysis.getCompleteTags(tagsExclusion);
      for (PageElementTag tag : tags) {
        PageElementFormatting.excludeArea(
            elements, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
      }
    }

    return elements;
  }

  /**
   * @param first First element.
   * @param second Second element.
   * @return True if both elements are in the same area.
   */
  public static boolean areInSameArea(
      PageElementFormatting first,
      PageElementFormatting second) {

    // Perform analysis on each element
    if (!first.analyzed) {
      first.analyze();
    }
    if (!second.analyzed) {
      second.analyze();
    }

    // Check if they are in the same area
    boolean sameArea = true;
    sameArea &= (first.inRefTag == second.inRefTag);
    sameArea &= (first.inILink == second.inILink);
    sameArea &= (first.inELink == second.inELink);
    sameArea &= (first.inTemplate == second.inTemplate);
    sameArea &= (first.inTemplateParameter == second.inTemplateParameter);
    sameArea &= (first.inTitle == second.inTitle);
    sameArea &= (first.inImage == second.inImage);
    sameArea &= (first.inListItem == second.inListItem);
    sameArea &= (first.inTable == second.inTable);
    sameArea &= (first.inTableCaption == second.inTableCaption);
    sameArea &= (first.inTableCell == second.inTableCell);
    // TODO
    return sameArea;
  }

  /**
   * Exclude an area from the analysis.
   * 
   * @param elements Formatting elements. 
   * @param beginIndex Begin index of the text area.
   * @param endIndex End index of the text area.
   */
  public static void excludeArea(
      List<PageElementFormatting> elements,
      int beginIndex, int endIndex) {
    Iterator<PageElementFormatting> itElement = elements.iterator();
    while (itElement.hasNext()) {
      PageElementFormatting element = itElement.next();
      if ((element.index >= beginIndex) &&
          (element.index + element.length <= endIndex)) {
        itElement.remove();
      }
    }
  }
}