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
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
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
    String templateNames = getSpecificProperty("templates", true, true, true);
    if ((templateNames != null) && (templateNames.trim().length() > 0)) {
      String[] templatesList = templateNames.split("\n");
      for (String templateName : templatesList) {
        templateName = templateName.trim();
        if (templateName.startsWith("{{")) {
          templateName = templateName.substring(2);
          if (templateName.endsWith("}}")) {
            templateName = templateName.substring(0, templateName.length() - 2);
          }
        }
        if (templateName.length() > 0) {
          List<PageElementTemplate> templates = analysis.getTemplates(templateName.trim());
          for (PageElementTemplate template : templates) {
            list.add(new TableElement(template.getBeginIndex(), template.getEndIndex(), false));
          }
        }
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
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH_CHEM, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
        (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
      return false;
    }
    if (analysis.isInComment(index) != null) {
      return false;
    }
    return true;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "templates", GT._("Templates that can replace the end of a table"));
    return parameters;
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
