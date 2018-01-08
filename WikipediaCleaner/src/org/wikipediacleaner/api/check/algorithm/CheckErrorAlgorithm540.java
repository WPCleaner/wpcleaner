/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.PageElementTable;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 540 of check wikipedia project.
 * Error 540: Missing end bold/italic (see [[Special:LintErrors/missing-end-tag]])
 */
public class CheckErrorAlgorithm540 extends CheckErrorAlgorithmBase {

  /** Possible global fixes */
  private final static String[] globalFixes = new String[] {
    GT._("Fix bold and italic"),
  };

  public CheckErrorAlgorithm540() {
    super("Missing bold/italic end tag");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze formatting elements
    boolean result = false;
    result |= analyzeFormatting(analysis, errors);

    return result;
  }

  // ==============================================================================================
  // Formatting elements analysis
  // ==============================================================================================

  /**
   * Analyze a page to check if formatting errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeFormatting(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Analyze contents to find formatting elements
    boolean result = false;
    List<FormattingElement> initialElements = FormattingElement.listFormattingElements(analysis);
    if (initialElements.isEmpty()) {
      return result;
    }
    List<FormattingElement> elements = new ArrayList<>(initialElements);

    // Remove correct formatting tags from the list
    List<FormattingElement> reportElements = new ArrayList<>();
    boolean shouldContinue = true;
    while (shouldContinue) {
      shouldContinue = false;

      // Check tags
      List<PageElementTag> tags = analysis.getTags();
      for (PageElementTag tag : tags) {
        if (tag.isComplete() && !tag.isFullTag()) {
          shouldContinue = analyzeCorrectArea(
              elements, reportElements,
              tag.getValueBeginIndex(), tag.getValueEndIndex(),
              tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
        }
      }

      // Check internal links
      List<PageElementInternalLink> iLinks = analysis.getInternalLinks();
      for (PageElementInternalLink iLink : iLinks) {
        int beginIndex = iLink.getBeginIndex();
        int endIndex = iLink.getEndIndex() - 2;
        if (iLink.getText() != null) {
          beginIndex += iLink.getTextOffset();
        } else {
          beginIndex =  endIndex;
        }
        shouldContinue |= analyzeCorrectArea(
            elements, reportElements,
            beginIndex, endIndex,
            iLink.getBeginIndex(), iLink.getEndIndex());
      }

      // Check images
      List<PageElementImage> images = analysis.getImages();
      for (PageElementImage image : images) {
        PageElementImage.Parameter paramDesc = image.getDescriptionParameter();
        int beginIndex = image.getBeginIndex();
        int endIndex = image.getBeginIndex();
        if (paramDesc != null) {
          beginIndex += paramDesc.getBeginOffset();
          endIndex += paramDesc.getEndOffset();
        }
        shouldContinue |= analyzeCorrectArea(
            elements, reportElements,
            beginIndex, endIndex,
            image.getBeginIndex(), image.getEndIndex());
      }

      // Check external links
      List<PageElementExternalLink> eLinks = analysis.getExternalLinks();
      for (PageElementExternalLink eLink : eLinks) {
        int beginIndex = eLink.getBeginIndex();
        int endIndex = eLink.getEndIndex() - 1;
        if (eLink.getText() != null) {
          beginIndex += eLink.getTextOffset();
        } else {
          beginIndex = endIndex;
        }
        shouldContinue |= analyzeCorrectArea(
            elements, reportElements,
            beginIndex, endIndex,
            eLink.getBeginIndex(), eLink.getEndIndex());
      }

      // Check titles
      List<PageElementTitle> titles = analysis.getTitles();
      for (PageElementTitle title : titles) {
        int beginIndex = title.getBeginIndex() + title.getFirstLevel();
        int endIndex = title.getEndIndex() - title.getSecondLevel();
        shouldContinue |= analyzeCorrectArea(
            elements, reportElements,
            beginIndex, endIndex,
            title.getBeginIndex(), title.getEndIndex());
      }

      // Check templates
      List<PageElementTemplate> templates = analysis.getTemplates();
      for (PageElementTemplate template : templates) {
        int beginIndex = template.getEndIndex() - 2;
        int endIndex = template.getEndIndex() - 2;
        if (template.getParameterCount() > 0) {
          beginIndex = template.getParameterPipeIndex(0);
        }
        shouldContinue |= analyzeCorrectArea(
            elements, reportElements,
            beginIndex, endIndex,
            template.getBeginIndex(), template.getEndIndex());
      }

      // Check list items
      List<PageElementListItem> items = analysis.getListItems();
      for (PageElementListItem item : items) {
        int beginIndex = item.getBeginIndex() + item.getDepth();
        int endIndex = item.getEndIndex();
        shouldContinue |= analyzeCorrectArea(
            elements, reportElements,
            beginIndex, endIndex,
            item.getBeginIndex(), item.getEndIndex());
      }

      // Check tables
      List<PageElementTable> tables = analysis.getTables();
      for (PageElementTable table : tables) {

        // Check table title
        PageElementTable.TableCaption caption = table.getTableCaption();
        if (caption != null) {
          shouldContinue |= analyzeCorrectArea(
              elements, reportElements,
              caption.getBeginIndex() + 2, caption.getEndIndex(),
              caption.getBeginIndex(), caption.getEndIndex());
        }

        // Check table cells
        for (PageElementTable.TableLine line : table.getTableLines()) {
          for (PageElementTable.TableCell cell : line.getCells()) {
            shouldContinue |= analyzeCorrectArea(
                elements, reportElements,
                cell.getEndOptionsIndex(), cell.getEndIndex(),
                cell.getBeginIndex(), cell.getEndIndex());
          }
        }
      }
    }

    // Report all errors
    for (FormattingElement element : reportElements) {
      if (errors == null) {
        return true;
      }
      result = true;
      reportFormattingElement(analysis, initialElements, element, errors);
    }

    // Report all formatting elements left
    //for (FormattingElement element : elements) {
    //  if (errors == null) {
    //    return true;
    //  }
    //  result = true;
    //  CheckErrorResult errorResult = createCheckErrorResult(
    //      analysis, element.index, element.index + element.length, ErrorLevel.WARNING);
    //  errors.add(errorResult);
    //}

    return result;
  }

  /**
   * Analyze an area to exclude correct parts.
   * 
   * @param elements List of formatting elements.
   * @param reportElements List of formatting elements to report.
   * @param beginAnalysis Begin index for the analysis.
   * @param endAnalysis End index for the analysis.
   * @param beginArea Begin index for the area.
   * @param endArea End index for the area.
   * @return True if modifications have been done.
   */
  private boolean analyzeCorrectArea(
      List<FormattingElement> elements,
      List<FormattingElement> reportElements,
      int beginAnalysis, int endAnalysis,
      int beginArea, int endArea) {

    // Analyze area for formatting elements
    FormattingAnalysis formattingArea = FormattingAnalysis.analyzeArea(elements, beginArea, endArea);
    if (formattingArea.boldCount + formattingArea.italicCount == 0) {
      return false;
    }

    // If only one, there's a problem that can already be reported
    if (formattingArea.boldCount + formattingArea.italicCount == 1) {
      for (FormattingElement element : formattingArea.elements) {
        reportElements.add(element);
      }
      FormattingElement.excludeArea(elements, beginArea, endArea);
      return true;
    }

    // Analyze area
    FormattingAnalysis formattingAnalysis = formattingArea;
    if ((beginAnalysis != beginArea) || (endAnalysis != endArea)) {
      formattingAnalysis = FormattingAnalysis.analyzeArea(elements, beginAnalysis, endAnalysis);
    }
    if (formattingAnalysis.boldCount + formattingAnalysis.italicCount == 0) {
      return false;
    }

    // If only one there's a problem that can already be reported
    if (formattingArea.boldCount + formattingArea.italicCount == 1) {
      for (FormattingElement element : formattingAnalysis.elements) {
        reportElements.add(element);
      }
      FormattingElement.excludeArea(elements, beginAnalysis, endAnalysis);
    }

    // Check that every element is in the same area
    FormattingElement firstElement = formattingAnalysis.elements.get(0);
    boolean sameArea = true;
    for (FormattingElement element : formattingAnalysis.elements) {
      sameArea &= FormattingElement.areInSameArea(firstElement, element);
    }
    if (!sameArea) {
      return false;
    }

    // Clear area if correct
    int countOutside =
        formattingArea.boldCount + formattingArea.italicCount -
        formattingAnalysis.boldCount - formattingAnalysis.italicCount;
    if ((formattingAnalysis.boldCount + formattingAnalysis.italicCount) % 2 == 0) {
      if (countOutside % 2 != 0) {
        FormattingElement.excludeArea(elements, beginAnalysis, endAnalysis);
      } else {
        FormattingElement.excludeArea(elements, beginArea, endArea);
      }
      return true;
    }

    // If all formats are the same, report the last one
    if ((formattingAnalysis.boldCount == 0) ||
        (formattingAnalysis.italicCount == 0)) {
      reportElements.add(formattingAnalysis.elements.get(formattingAnalysis.elements.size() - 1));
      if (countOutside % 2 != 0) {
        FormattingElement.excludeArea(elements, beginAnalysis, endAnalysis);
      } else {
        FormattingElement.excludeArea(elements, beginArea, endArea);
      }
      return true;
    }

    // Report one
    reportElements.add(formattingAnalysis.elements.get(formattingAnalysis.elements.size() - 1));
    if (countOutside % 2 != 0) {
      FormattingElement.excludeArea(elements, beginAnalysis, endAnalysis);
    } else {
      FormattingElement.excludeArea(elements, beginArea, endArea);
    }
    return true;
  }

  /**
   * @param analysis Page analysis.
   * @param elements Formatting elements.
   * @param element Formatting element to report.
   * @param errors List of errors.
   */
  private void reportFormattingElement(
      PageAnalysis analysis,
      List<FormattingElement> elements,
      FormattingElement element,
      Collection<CheckErrorResult> errors) {

    // Report inside a list item
    PageElementListItem listItem = element.isInListItem();
    if (listItem != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          listItem.getBeginIndex() + listItem.getDepth(), listItem.getEndIndex(),
          listItem.getBeginIndex(), listItem.getEndIndex(),
          true, false, false, true)) {
        return;
      }
    }

    // Report inside an internal link
    PageElementInternalLink iLink = element.isInInternalLink();
    if (iLink != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          iLink.getBeginIndex() + iLink.getTextOffset(), iLink.getEndIndex() - 2,
          iLink.getBeginIndex(), iLink.getEndIndex(),
          false, false, true, true)) {
        return;
      }
    }

    // Report inside an image
    PageElementImage image = element.isInImage();
    if (image != null) {
      PageElementImage.Parameter paramDesc = image.getDescriptionParameter();
      if (paramDesc != null) {
        if (reportFormattingElement(
            analysis, elements, element, errors,
            image.getBeginIndex() + paramDesc.getBeginOffset(),
            image.getBeginIndex() + paramDesc.getEndOffset(),
            image.getBeginIndex() + paramDesc.getBeginOffset() - 1,
            image.getBeginIndex() + paramDesc.getEndOffset(),
            true, false, true, true)) {
          return;
        }
      }
    }

    // Report inside an external link
    PageElementExternalLink eLink = element.isInExternalLink();
    if (eLink != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          eLink.getBeginIndex() + eLink.getTextOffset(), eLink.getEndIndex() - 1,
          eLink.getBeginIndex(), eLink.getEndIndex(),
          false, false, true, true)) {
        return;
      }
    }

    // Report inside a title
    PageElementTitle title = element.isInTitle();
    if (title != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          title.getBeginIndex() + title.getFirstLevel(),
          title.getEndIndex() - title.getSecondLevel(),
          title.getBeginIndex(), title.getEndIndex(),
          true, false, true, true)) {
        return;
      }
    }

    // Report inside a reference tag
    PageElementTag refTag = element.isInRefTag();
    if (refTag != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          refTag.getValueBeginIndex(), refTag.getValueEndIndex(),
          refTag.getValueBeginIndex(), refTag.getValueEndIndex(),
          false, true, false, true)) {
        return;
      }
    }

    // Report inside a table caption
    PageElementTable.TableCaption caption = element.isInTableCaption();
    if (caption != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          caption.getBeginIndex() + 2, caption.getEndIndex(),
          caption.getBeginIndex(), caption.getEndIndex(),
          true, false, true, true)) {
        return;
      }
    }

    // Report inside a table cell
    PageElementTable.TableCell cell = element.isInTableCell();
    if (cell != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          cell.getEndOptionsIndex(), cell.getEndIndex(),
          cell.getEndOptionsIndex(), cell.getEndIndex(),
          true, false, true, true)) {
        return;
      }
    }

    // Report inside a template
    PageElementTemplate.Parameter templateParam = element.isInTemplateParameter();
    if (templateParam != null) {
      if (reportFormattingElement(
          analysis, elements, element, errors,
          templateParam.getValueStartIndex(), templateParam.getEndIndex(),
          templateParam.getValueStartIndex(), templateParam.getEndIndex(),
          true, false, true, true)) {
        return;
      }
    }

    // Default report
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, element.index, element.index + element.length);
    errors.add(errorResult);
  }

  /**
   * @param analysis Page analysis.
   * @param elements List of formatting elements.
   * @param element Formatting element to report.
   * @param errors List of errors.
   * @param beginIndex Begin index of the area.
   * @param endIndex End index of the are.
   * @return True if the error has been reported.
   */
  private boolean reportFormattingElement(
      PageAnalysis analysis,
      List<FormattingElement> elements,
      FormattingElement element,
      Collection<CheckErrorResult> errors,
      int beginIndex, int endIndex,
      int beginArea, int endArea,
      boolean deleteEmpty, boolean requiresText,
      boolean closeFull, boolean deleteEnd) {

    // Reduce area
    beginIndex = moveBeginIndex(analysis, beginIndex, endIndex);
    endIndex = moveEndIndex(analysis, beginIndex, endIndex);
    beginArea = moveBeginIndex(analysis, beginArea, endArea);
    endArea = moveEndIndex(analysis, beginArea, endArea);

    // Check a few things
    boolean hasSingleQuote = false;
    boolean hasDoubleQuotes = false;
    String contents = analysis.getContents();
    for (int index = beginIndex; index < endIndex; index++) {

      // Single quotes
      if (contents.charAt(index) == '\'') {
        if (((index <= beginIndex) ||
             (contents.charAt(index - 1) != '\'')) &&
            ((index + 1 >= endIndex) ||
             (contents.charAt(index + 1) != '\''))) {
          boolean shouldCount = true;
          if (shouldCount) {
            if ((element.isInInternalLink() == null) &&
                (analysis.isInInternalLink(index) != null)) {
              shouldCount = false;
            }
          }
          if (shouldCount) {
            hasSingleQuote = true;
          }
        }
      }

      // Double quotes
      if ("\"“".indexOf(contents.charAt(index)) >= 0) {
        hasDoubleQuotes = true;
      }
    }

    // Report with only one formatting element
    FormattingAnalysis formatting = FormattingAnalysis.analyzeArea(
        elements, beginIndex, endIndex);
    if (formatting.elements.size() == 1) {

      // Report with only the formatting element
      if ((element.index == beginIndex) &&
          (element.index + element.length == endIndex)) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginArea, endArea);
        if (requiresText &&
            ((beginArea == 0) || (contents.charAt(beginIndex - 1) != ' ')) &&
            ((endArea >= contents.length()) || (contents.charAt(endIndex) !=  ' '))) {
          errorResult.addReplacement(" ", deleteEmpty);
        } else {
          errorResult.addReplacement("", deleteEmpty);
        }
        errors.add(errorResult);
        return true;
      }

      // Report with the formatting element at the beginning
      if (element.index == beginIndex) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, element.index, endIndex);
        String addition = contents.substring(
            element.index, element.index + element.getMeaningfulLength()); 
        String replacement =
            contents.substring(element.index, endIndex) +
            addition;
        String text = addition + "..." + addition;
        closeFull &= !hasSingleQuote;
        closeFull &= !hasDoubleQuotes;
        closeFull &= (contents.charAt(endIndex - 1) != '\'');
        closeFull &= element.isAloneInArea(elements);
        errorResult.addReplacement(replacement, text, closeFull);
        errors.add(errorResult);
        return true;
      }

      // Report with the formatting element at the end
      if (element.index + element.length == endIndex) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, element.index, element.index + element.length);
        deleteEnd &= !hasSingleQuote;
        deleteEnd &= !hasDoubleQuotes;
        deleteEnd &= element.isAloneInArea(elements);
        errorResult.addReplacement("", deleteEnd);
        errors.add(errorResult);
        return true;
      }
    }

    return false;
  }

  /**
   * @param analysis Page analysis.
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @return New begin index with eventually reduced area.
   */
  private int moveBeginIndex(
      PageAnalysis analysis, int beginIndex, int endIndex) {
    String contents = analysis.getContents();
    boolean tryAgain = false;
    do {
      tryAgain = false;

      // Ignore whitespace at the beginning
      while ((beginIndex < endIndex) &&
             (" \n".indexOf(contents.charAt(beginIndex)) >= 0)) {
        beginIndex++;
        tryAgain = true;
      }

      // Ignore templates at the beginning
      if ((beginIndex < endIndex) && (contents.charAt(beginIndex) == '{')) {
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if ((template != null) && (template.getBeginIndex() == beginIndex)) {
          if (template.getEndIndex() < endIndex) {
            beginIndex = template.getEndIndex();
            tryAgain = true;
          }
        }
      }

      // Ignore unclosed tags at the beginning
      if ((beginIndex < endIndex) && (contents.charAt(beginIndex) == '<')) {
        PageElementTag tag = analysis.isInTag(beginIndex);
        if ((tag != null) && (tag.getBeginIndex() == beginIndex)) {
          if (tag.isFullTag() || !tag.isComplete()) {
            beginIndex = tag.getEndIndex();
            tryAgain = true;
          }
        }
      }

      // Ignore comments at the beginning
      if ((beginIndex < endIndex) && (contents.charAt(beginIndex) == '<')) {
        PageElementComment comment = analysis.isInComment(beginIndex);
        if ((comment != null) && (comment.getBeginIndex() == beginIndex)) {
          beginIndex = comment.getEndIndex();
          tryAgain = true;
        }
      }
    } while (tryAgain);
    return beginIndex;
  }

  /**
   * @param analysis Page analysis.
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @return New end index with eventually reduced area.
   */
  private int moveEndIndex(
      PageAnalysis analysis, int beginIndex, int endIndex) {
    String contents = analysis.getContents();
    boolean tryAgain = false;
    do {
      tryAgain = false;

      // Ignore whitespace at the end
      while ((endIndex > beginIndex) &&
             (" \n".indexOf(contents.charAt(endIndex - 1)) >= 0)) {
        endIndex--;
        tryAgain = true;
      }

      // Ignore comments at the end
      if ((endIndex > beginIndex) && (contents.charAt(endIndex - 1) == '>')) {
        PageElementComment comment = analysis.isInComment(endIndex - 1);
        if ((comment != null) && (comment.getEndIndex() == endIndex)) {
          endIndex = comment.getBeginIndex();
          tryAgain = true;
        }
      }
    } while (tryAgain);
    return endIndex;
  }

  /**
   * Bean for memorizing formatting elements
   */
  private static class FormattingElement {

    /** Page analysis */
    private final PageAnalysis analysis;

    /** Index of the formatting element in the text */
    final int index;

    /** Length of the formatting element */
    final int length;

    /** True when element has been analyzed */
    private boolean analyzed;

    /** <ref> tag in which the element is */
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
    private FormattingElement(
        PageAnalysis analysis, int index, int length) {
      this.analysis = analysis;
      this.index = index;
      this.length = length;
      this.analyzed = false;
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
      // TODO: more analysis
      analyzed = true;
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
     * @param elements Elements.
     * @return True if an other element is in the same are.
     */
    public boolean isAloneInArea(List<FormattingElement> elements) {
      if (elements != null) {
        for (FormattingElement element : elements) {
          if (element != this) {
            boolean checked = false;

            // Check inside a reference tag
            if (!checked) {
              if (inRefTag != null) {
                if ((element.index >= inRefTag.getValueBeginIndex()) &&
                    (element.index < inRefTag.getValueEndIndex())) {
                  return false;
                }
                checked = true;
              } else if (element.inRefTag != null) {
                checked = true;
              }
            }

            // Check inside a title
            if (!checked) {
              if (inTitle != null) {
                if (inTitle.containsIndex(element.index)) {
                  return false;
                }
                checked = true;
              } else if (element.inTitle != null) {
                checked = true;
              }
            }

            // Check inside an image
            if (!checked) {
              if (inImage != null) {
                if (inImage.containsIndex(element.index)) {
                  return false;
                }
                checked = true;
              } else if (element.inImage != null) {
                checked = true;
              }
            }

            // Check inside a table caption
            if (!checked) {
              if ((inTable != null) && (inTableCaption != null)) {
                if (inTableCaption.containsIndex(element.index)) {
                  return false;
                }
                checked = true;
              } else if (element.inTableCaption != null) {
                checked = true;
              }
            }

            // Check inside a table cell
            if (!checked) {
              if ((inTable != null) && (inTableCell != null)) {
                if (inTableCell.containsIndex(element.index)) {
                  return false;
                }
                checked = true;
              } else if (element.inTableCell != null) {
                checked = true;
              }
            }

            // Check the rest of the elements
            if (!checked) {
              if ((inILink != null) && (inILink.containsIndex(element.index))) {
                return false;
              }
              if ((inELink != null) && (inELink.containsIndex(element.index))) {
                return false;
              }
              if ((inListItem != null) && (inListItem.containsIndex(element.index))) {
                return false;
              }
              if ((inTemplate != null) && (inTemplate.containsIndex(element.index))) {
                return false;
              }
              // TODO: change to true once paragraph is managed
              return false;
            }
          }
        }
      }
      return true;
    }

    /**
     * @param analysis Page analysis.
     * @return List of formatting elements in the page.
     */
    static List<FormattingElement> listFormattingElements(
        PageAnalysis analysis) {

      // Analyze contents for formatting elements
      List<FormattingElement> elements = new ArrayList<>();
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
            elements.add(new FormattingElement(analysis, index, length));
          }
          index += length;
        }
      } while (index >= 0);

      // Exclude comments
      List<PageElementComment> comments = analysis.getComments();
      for (PageElementComment comment : comments) {
        FormattingElement.excludeArea(
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
          FormattingElement.excludeArea(
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
    static boolean areInSameArea(
        FormattingElement first,
        FormattingElement second) {

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
    static void excludeArea(
        List<FormattingElement> elements,
        int beginIndex, int endIndex) {
      Iterator<FormattingElement> itElement = elements.iterator();
      while (itElement.hasNext()) {
        FormattingElement element = itElement.next();
        if ((element.index >= beginIndex) &&
            (element.index + element.length <= endIndex)) {
          itElement.remove();
        }
      }
    }
  }

  /**
   * Bean for storing the analysis of formatting elements in an area.
   */
  private static class FormattingAnalysis {

    /** List of formatting elements */
    final List<FormattingElement> elements;

    /** Count of bold formatting */
    final int boldCount;

    /** Count of italic formatting */
    final int italicCount;

    /** Static object for empty analysis to avoid useless memory allocation */
    final private static FormattingAnalysis EMPTY = new FormattingAnalysis(new ArrayList<FormattingElement>(), 0, 0);

    /**
     * @param bold Count of bold formatting.
     * @param italic Count of italic formatting.
     */
    private FormattingAnalysis(
        List<FormattingElement> elements,
        int bold, int italic) {
      this.elements = elements;
      this.boldCount = bold;
      this.italicCount = italic;
    }

    /**
     * Analyze an area for its formatting elements.
     * 
     * @param elements Formatting elements.
     * @param beginIndex Begin index of the text area.
     * @param endIndex End index of the text area.
     * @return Analysis.
     */
    static FormattingAnalysis analyzeArea(
        List<FormattingElement> elements,
        int beginIndex, int endIndex) {
      int bold = 0;
      int italic = 0;
      List<FormattingElement> selected = null;
      for (int index = 0; index < elements.size(); index++) {
        FormattingElement element = elements.get(index);
        if ((element.index >= beginIndex) &&
            (element.index + element.length <= endIndex)) {
          if (selected == null) {
            selected = new ArrayList<>();
          }
          selected.add(element);
          if (element.isBold()) {
            bold++;
          }
          if (element.isItalic()) {
            italic++;
          }
        }
      }
      if (bold + italic == 0) {
        return FormattingAnalysis.EMPTY;
      }
      return new FormattingAnalysis(selected, bold, italic);
    }
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
