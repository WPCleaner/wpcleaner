/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 28 of check wikipedia project.
 * Error 28: Table not correct end
 */
public class CheckErrorAlgorithm028 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm028() {
    super("Table not correct end");
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

    // Retrieve list of table starts
    List<TableElement> starts = getTableStarts(analysis);
    if (starts.isEmpty()) {
      return false;
    }

    // Retrieve list of table ends and pair them
    List<TableElement> ends = getTableEnds(analysis);
    for (TableElement end : ends) {
      TableElement pair = null;
      for (TableElement start : starts) {
        if (!start.hasMatch && (start.endIndex < end.beginIndex)) {
          pair = start;
        }
      }
      if (pair != null) {
        pair.hasMatch = true;
        end.hasMatch = true;
      }
    }

    // Check that every start has a matching end
    boolean result = false;
    for (TableElement start : starts) {
      if (!start.hasMatch) {
        result = true;
      }
    }
    if (!result || (errors == null)) {
      return result;
    }

    // Mark errors
    starts.addAll(ends);
    Collections.sort(starts);
    for (TableElement element : starts) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, element.beginIndex, element.endIndex,
          (element.begin && !element.hasMatch) ? ErrorLevel.ERROR : ErrorLevel.CORRECT);
      errors.add(errorResult);
    }
    return result;
  }

  /**
   * Construct list of table starts.
   * 
   * @param analysis Page analysis.
   * @return List of table starts.
   */
  private List<TableElement> getTableStarts(PageAnalysis analysis) {
    List<TableElement> list = new ArrayList<CheckErrorAlgorithm028.TableElement>();

    // Find tables beginning by {|
    String contents = analysis.getContents();
    int index = contents.indexOf("{|");
    while (index >= 0) {
      if ((index == 0) || (contents.charAt(index - 1) != '{')) {
        if (shouldCount(analysis, index)) {
          list.add(new TableElement(index, index + 2, true));
        }
      }
      index = contents.indexOf("{|", index + 1);
    }

    // Find tables beginning by <table>
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_HTML_TABLE);
    for (PageElementTag tag : tags) {
      if (!tag.isFullTag() && !tag.isEndTag()) {
        if (shouldCount(analysis, index)) {
          list.add(new TableElement(tag.getBeginIndex(), tag.getEndIndex(), true));
        }
      }
    }

    Collections.sort(list);
    return list;
  }

  /**
   * Construct list of table ends.
   * 
   * @param analysis Page analysis.
   * @return List of table ends.
   */
  private List<TableElement> getTableEnds(PageAnalysis analysis) {
    List<TableElement> list = new ArrayList<CheckErrorAlgorithm028.TableElement>();

    // Find tables ending by |}
    String contents = analysis.getContents();
    int index = contents.indexOf("|}");
    while (index >= 0) {
      if (shouldCount(analysis, index)) {
        PageElementTemplate template = analysis.isInTemplate(index);
        if ((template == null) ||
            (template.getEndIndex() != index + 3)) {
          list.add(new TableElement(index, index + 2, false));
        }
      }
      index = contents.indexOf("|}", index + 1);
    }

    // Find tables ending by </table>
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_HTML_TABLE);
    for (PageElementTag tag : tags) {
      if (!tag.isFullTag() && tag.isEndTag()) {
        if (shouldCount(analysis, index)) {
          list.add(new TableElement(tag.getBeginIndex(), tag.getEndIndex(), false));
        }
      }
    }

    // Find tables ending by a template
    for (String templateName : templateNames) {
      List<PageElementTemplate> templates = analysis.getTemplates(templateName);
      for (PageElementTemplate template : templates) {
        list.add(new TableElement(template.getBeginIndex(), template.getEndIndex(), false));
      }
    }

    Collections.sort(list);
    return list;
  }

  /**
   * @param analysis Page analysis.
   * @param index Current index.
   * @return True if this place should count for the detection.
   */
  private boolean shouldCount(PageAnalysis analysis, int index) {
    if ((analysis.getSurroundingTag(PageElementTag.TAG_HTML_CODE, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_CHEM, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH_CHEM, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
      return false;
    }
    if (analysis.comments().isAt(index)) {
      return false;
    }
    return true;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates that can replace the end of a table */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, true);
    templateNames.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpName : tmpList) {
          tmpName = tmpName.trim();
          if (tmpName.startsWith("{{")) {
            tmpName = tmpName.substring(2);
            if (tmpName.endsWith("}}")) {
              tmpName = tmpName.substring(0,  tmpName.length() - 2);
            }
          }
          tmpName = tmpName.trim();
          if (tmpName.length() > 0) {
            templateNames.add(tmpName);
          }
        }
      }
    }
  }

  /** Templates that can replace the end of a table */
  private final List<String> templateNames = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that can replace the end of a table"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template that can replace the end of a table")),
        true));
  }

  /**
   * POJO for storing table begin and end.
   */
  private static class TableElement implements Comparable<TableElement> {
    public final int beginIndex;
    public final int endIndex;
    public final boolean begin;
    public boolean hasMatch;

    TableElement(int beginIndex, int endIndex, boolean begin) {
      this.beginIndex = beginIndex;
      this.endIndex = endIndex;
      this.begin = begin;
    }

    /**
     * @param o
     * @return
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(TableElement o) {
      return beginIndex - o.beginIndex;
    }
  }
}
