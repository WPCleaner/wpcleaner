/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a05x.a055;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.PageElementTable;
import org.wikipediacleaner.api.data.PageElementTable.TableCell;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.Interval;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 55 of check wikipedia project.
 * Error 55: HTML text style element &lt;small&gt; double
 */
public class CheckErrorAlgorithm055 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm055() {
    super("HTML text style element <small> double");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyzing the text from the beginning
    List<PageElementTag> tags = analysis.getTags(HtmlTagType.SMALL);
    if (tags == null) {
      return false;
    }
    AnalysisSituation situation = new AnalysisSituation();
    boolean result = false;
    int tagIndex = 0;
    while (tagIndex < tags.size()) {
      PageElementTag tag = tags.get(tagIndex);
      tagIndex++;

      if (tag.isFullTag()) {
        result |= analyzeFullTag(analysis, errors, situation, tag);
      } else if (tag.isEndTag()) {
        result |= analyzeEndTag(analysis, errors, situation, tag);
      } else {
        result |= analyzeBeginTag(analysis, errors, situation, tag, tags, tagIndex);
      }

      // Memorize if tag is unclosed
      situation.previousUnclosedTag = !tag.isComplete() && !tag.isFullTag() && !tag.isEndTag();
    }

    return result;
  }

  /**
   * Analyze a begin tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param situation Situation of the analysis.
   * @param tag Begin tag.
   * @param tags All tags
   * @param tagIndex Index of the tag in the list of small tags
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeBeginTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      AnalysisSituation situation,
      PageElementTag tag,
      List<PageElementTag> tags,
      int tagIndex) {

    // Handle a first tag
    if (situation.level == 0) {
      situation.level0Tag = tag;
      situation.level++;
      return false;
    }

    // Check if the tag should be reported
    situation.level++;
    if (!shouldReport(analysis, tag)) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Manage double small tags on the same text
    boolean doubleSmall = false;
    if ((tag.getMatchingTag() != null) &&
        (situation.level0Tag != null) &&
        (situation.level0Tag.getMatchingTag() != null)) {
      if ((situation.level0Tag.getEndIndex() == tag.getBeginIndex()) &&
          (tag.getMatchingTag().getEndIndex() == situation.level0Tag.getMatchingTag().getBeginIndex())) {
        doubleSmall = true;
      }
    }

    String contents = analysis.getContents();
    if (situation.level0Tag != null) {
      int possibleEnd = situation.previousUnclosedTag ? getPossibleEnd(analysis, situation.level0Tag) : 0;
      if (possibleEnd > 0) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            situation.level0Tag.getBeginIndex(), possibleEnd,
            ErrorLevel.WARNING);
        errorResult.addReplacement(
            contents.substring(situation.level0Tag.getBeginIndex(), possibleEnd) + HtmlTagType.SMALL.getCloseTag(),
            HtmlTagType.SMALL.getCompleteTag());
        errors.add(errorResult);
      } else {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            situation.level0Tag.getBeginIndex(),
            situation.level0Tag.getEndIndex(),
            ErrorLevel.WARNING);
        errors.add(errorResult);
        if (situation.level0Tag.getMatchingTag() != null) {
          errorResult = createCheckErrorResult(
              analysis,
              situation.level0Tag.getMatchingTag().getBeginIndex(),
              situation.level0Tag.getMatchingTag().getEndIndex(),
              ErrorLevel.WARNING);
          errors.add(errorResult);
        }
      }
      situation.level0Tag = null;
    }

    int possibleEnd = getPossibleEnd(analysis, tag);
    if (possibleEnd > 0) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, tag.getBeginIndex(), possibleEnd);
      errorResult.addReplacement(
          contents.substring(tag.getBeginIndex(), possibleEnd) + HtmlTagType.SMALL.getCloseTag(),
          HtmlTagType.SMALL.getCompleteTag());
      errors.add(errorResult);
    } else {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis,
          tag.getCompleteBeginIndex(),
          tag.getCompleteEndIndex());
      if (doubleSmall) {
        errorResult.addReplacement(
            contents.substring(tag.getEndIndex(), tag.getMatchingTag().getBeginIndex()),
            GT._T("Remove {0} tags", HtmlTagType.SMALL.getOpenTag()));
      }
      if (!tag.isComplete() && !tag.isFullTag() && !tag.isEndTag() && situation.previousUnclosedTag) {
        errorResult.addReplacement(HtmlTagType.SMALL.getCloseTag());
      }
      errors.add(errorResult);
      if (tag.isComplete()) {
        tagIndex = PageElementTag.getMatchingTagIndex(tags, tagIndex);
      }
    }

    return true;
  }

  /**
   * Analyze an end tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param situation Situation of the analysis.
   * @param tag End tag.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeEndTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      AnalysisSituation situation,
      PageElementTag tag) {

    // Check if an error is present
    situation.level--;
    if (situation.level >= 0) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Report error
    situation.level = 0;
    int beginIndex = tag.getBeginIndex();
    int endIndex = tag.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    List<Interval> intervals = new ArrayList<>();
    PageElementTable table = analysis.isInTable(beginIndex);
    if (table != null) {
      TableCell cell = table.getCellAtIndex(beginIndex);
      if (cell != null) {
        intervals.add(cell);
      }
    }
    PageElementListItem listItem = analysis.isInListItem(beginIndex);
    if (listItem != null) {
      intervals.add(listItem);
    }
    boolean automatic = false;
    for (Interval interval : intervals) {
      String contents = analysis.getContents();
      boolean automaticInterval = true;
      int intervalBeginIndex = interval.getBeginIndex();
      int intervalEndIndex = interval.getEndIndex();
      while ((intervalEndIndex > intervalBeginIndex) &&
             (" \n".indexOf(contents.charAt(intervalEndIndex - 1)) >= 0)) {
        intervalEndIndex--;
      }
      for (int tmpIndex = intervalBeginIndex; tmpIndex < intervalEndIndex; tmpIndex++) {
        if ((tmpIndex < beginIndex) || (tmpIndex >= endIndex)) {
          if (contents.startsWith("small", tmpIndex) ||
              ("<>/\n".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            automaticInterval = false;
          }
        }
      }
      automatic |= automaticInterval;
      
    }
    errorResult.addReplacement("", automatic);
    errors.add(errorResult);
    return true;
  }

  /**
   * Analyze a full tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param situation Situation of the analysis.
   * @param tag Full tag.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeFullTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      AnalysisSituation situation,
      PageElementTag tag) {

    // Check if an error is present
    if (situation.level <= 0) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        tag.getBeginIndex(), tag.getEndIndex());
    if (situation.previousUnclosedTag) {
      errorResult.addReplacement(HtmlTagType.SMALL.getCloseTag());
    }
    errorResult.addReplacement("");
    errors.add(errorResult);
    return true;
  }

  private boolean shouldReport(
      PageAnalysis analysis,
      PageElementTag tag) {

    // All problems are reported in namespaces where signatures are not expected
    if (analysis.getPage().isArticle() &&
        (Namespace.IMAGE != analysis.getPage().getNamespace())) {
      return true;
    }

    // Do not report if tag is in a link to user namespace
    PageElementInternalLink link = analysis.isInInternalLink(tag.getBeginIndex());
    if (link != null) {
      int namespace = link.getNamespace(analysis.getWikipedia());
      if ((Namespace.USER == namespace) ||
          (Namespace.USER_TALK == namespace)) {
        return false;
      }
    }

    return true;
  }

  /**
   * Find a possible end for a single small tag.
   * 
   * @param analysis Page analysis.
   * @param tag Current small tag.
   * @return Possible end if found, -1 otherwise.
   */
  private int getPossibleEnd(
      PageAnalysis analysis,
      PageElementTag tag) {

    // Check arguments
    if ((analysis == null) || (analysis.getContents() == null)) {
      return -1;
    }
    if ((tag == null) || tag.isComplete() || tag.isEndTag()) {
      return -1;
    }

    // Check various cases
    int possibleEnd = -1;
    int tmp = getPossibleEndInTable(analysis, tag);
    if ((tmp > 0) && ((possibleEnd <= 0) || (tmp < possibleEnd))) {
      possibleEnd = tmp;
    }
    tmp = getPossibleEndInTemplate(analysis, tag);
    if ((tmp > 0) && ((possibleEnd <= 0) || (tmp < possibleEnd))) {
      possibleEnd = tmp;
    }
    tmp = getPossibleEndInGallery(analysis, tag);
    if ((tmp > 0) && ((possibleEnd <= 0) || (tmp < possibleEnd))) {
      possibleEnd = tmp;
    }

    // Check that there is no other small tag in the selected area
    if (possibleEnd > 0) {
      List<PageElementTag> tags = analysis.getTags(HtmlTagType.SMALL);
      for (PageElementTag tmpTag : tags) {
        if ((tmpTag.getBeginIndex() >= tag.getEndIndex()) &&
            (tmpTag.getBeginIndex() < possibleEnd)) {
          return -1;
        }
      }
    }

    // Restrict selection
    if (possibleEnd > 0) {
      String contents = analysis.getContents();
      boolean finished = false;
      while (!finished) {
        finished = true;
        if (possibleEnd > 0) {
          char previousChar = contents.charAt(possibleEnd - 1);
          if (" \n".indexOf(previousChar) >= 0) {
            possibleEnd--;
            finished = false;
          } else if (previousChar == '>') {
            PageElementTag tmpTag = analysis.isInTag(possibleEnd - 1);
            if (tmpTag != null) {
              int completeBegin = tmpTag.getCompleteBeginIndex();
              if (HtmlTagType.BR.equals(tmpTag.getType())) {
                possibleEnd = completeBegin;
                finished = false;
              } else if (WikiTagType.REF.equals(tmpTag.getType())) {
                while ((tmpTag != null) &&
                       (WikiTagType.REF.equals(tmpTag.getType()))) {
                  previousChar = contents.charAt(completeBegin - 1);
                  if ("},.;:!".indexOf(previousChar) >= 0) {
                    tmpTag = null;
                  } else if (previousChar == '>') {
                    tmpTag = analysis.isInTag(completeBegin - 1);
                    if (tmpTag == null) {
                      possibleEnd = completeBegin;
                      finished = false;
                    } else if (!WikiTagType.REF.equals(tmpTag.getType())) {
                      possibleEnd = tmpTag.getCompleteBeginIndex();
                      finished = false;
                    } else {
                      tmpTag = null;
                    }
                  } else {
                    tmpTag = null;
                    possibleEnd = completeBegin;
                    finished = false;
                  }
                }
              }
            }
          }
        }
      }
      if (possibleEnd <= tag.getEndIndex()) {
        return -1;
      }
    }

    return possibleEnd;
  }

  /**
   * Find a possible end for a single small tag in a table.
   * 
   * @param analysis Page analysis.
   * @param tag Current small tag.
   * @return Possible end if found, -1 otherwise.
   */
  private int getPossibleEndInTable(
      PageAnalysis analysis,
      PageElementTag tag) {
    String contents = analysis.getContents();

    // Check the beginning of the line
    int tmpIndex = tag.getBeginIndex();
    while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) != '\n')) {
      tmpIndex--;
    }
    if ((contents.charAt(tmpIndex) != '|') &&
        (contents.charAt(tmpIndex) != '!')) {
      return -1;
    }

    // Check the following characters
    tmpIndex = tag.getEndIndex();
    boolean finished = false;
    while ((tmpIndex < contents.length()) && !finished) {
      int nextIndex = tmpIndex + 1;
      if ((contents.charAt(tmpIndex) == '\n') ||
          (contents.startsWith("||", tmpIndex)) ||
          (contents.startsWith("!!", tmpIndex))) {
        finished = true;
      }
      if (contents.charAt(tmpIndex) == '{') {
        PageElementTemplate template = analysis.isInTemplate(tmpIndex);
        if ((template != null) && (template.getBeginIndex() == tmpIndex)) {
          nextIndex = template.getEndIndex();
        } else {
          PageElementFunction function = analysis.isInFunction(tmpIndex);
          if ((function != null) && (function.getBeginIndex() == tmpIndex)) {
            nextIndex = function.getEndIndex();
          }
        }
      }
      if (!finished) {
        tmpIndex = nextIndex;
      }
    }
    if ((contents.charAt(tmpIndex) != '\n') &&
        (!contents.startsWith("||", tmpIndex))) {
      return -1;
    }

    return tmpIndex;
  }

  /**
   * Find a possible end for a single small tag in a template.
   * 
   * @param analysis Page analysis.
   * @param tag Current small tag.
   * @return Possible end if found, -1 otherwise.
   */
  private int getPossibleEndInTemplate(
      PageAnalysis analysis,
      PageElementTag tag) {

    // Check if in template
    PageElementTemplate template = analysis.isInTemplate(tag.getBeginIndex());
    if (template == null) {
      return -1;
    }
    Parameter param = template.getParameterAtIndex(tag.getBeginIndex());
    if (param == null) {
      return -1;
    }
    return param.getEndIndex();
  }

  /**
   * Find a possible end for a single small tag in a gallery.
   * 
   * @param analysis Page analysis.
   * @param tag Current small tag.
   * @return Possible end if found, -1 otherwise.
   */
  private int getPossibleEndInGallery(
      PageAnalysis analysis,
      PageElementTag tag) {

    // Check if in gallery
    PageElementTag tagGallery = analysis.getSurroundingTag(
        WikiTagType.GALLERY, tag.getBeginIndex());
    if (tagGallery == null) {
      return -1;
    }

    // Check that nothing prevents from closing the tag at the end of the line
    int index = tag.getValueBeginIndex();
    String contents = analysis.getContents();
    while (index < contents.length()) {
      char currentChar = contents.charAt(index);
      if (currentChar == '\n') {
        return index;
      }
      if ("|<{".indexOf(currentChar) >= 0) {
        return -1;
      }
      index++;
    }

    return -1;
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
