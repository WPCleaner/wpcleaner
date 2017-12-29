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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
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
    List<FormattingElement> elements = FormattingElement.listFormattingElements(analysis);
    if (elements.isEmpty()) {
      return result;
    }

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
      // Check tables
      List<PageElementTable> tables = analysis.getTables();
      for (PageElementTable table : tables) {
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
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, element.index, element.index + element.length, ErrorLevel.ERROR);
      errors.add(errorResult);
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
    if ((formattingAnalysis.boldCount + formattingAnalysis.italicCount) % 2 == 0) {
      int countOutside =
          formattingArea.boldCount + formattingArea.italicCount -
          formattingAnalysis.boldCount - formattingAnalysis.italicCount;
      if (countOutside % 2 != 0) {
        FormattingElement.excludeArea(elements, beginAnalysis, endAnalysis);
      } else {
        FormattingElement.excludeArea(elements, beginArea, endArea);
      }
      return true;
    }

    return false;
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
      // TODO: more analysis
      analyzed = true;
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
