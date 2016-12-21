/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 112 of check wikipedia project.
 * Error 112: Bad or deprecated CSS attributes
 */
public class CheckErrorAlgorithm112 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm112() {
    super("Bad or deprecated CSS attributes");
  }

  /** List of attributes for each kind of tag */
  private final static Map<String, String[]> TAGS = new HashMap<>();

  /** List of attributes for tables */
  private final static String[] TABLE_ATTRIBUTES = new String[] {
    "contenteditable",
    "data-cx-weight",
    "data-source",
    "id",
  };

  static {
    TAGS.put(PageElementTag.TAG_HTML_CENTER, new String[] {
        "contenteditable",
        "data-cx-weight",
        "data-source",
        "id",
    });
    TAGS.put(PageElementTag.TAG_HTML_DIV, new String[] {
        "-moz-border-radius",
        "-moz-box-shadow",
        "-moz-column-count",
        "-moz-column-gap",
        "-moz-column-width",
        "-moz-linear-gradient",
        "-webkit-column-count",
    });
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

    // Analyze each kind of tags
    boolean result = false;
    for (Entry<String, String[]> tag : TAGS.entrySet()) {
      result |= analyzeTag(analysis, errors, tag.getKey(), tag.getValue());
      if (result && (errors == null)) {
        return result;
      }
    }
    result |= analyzeTables(analysis, errors, TABLE_ATTRIBUTES);

    return result;
  }

  /**
   * Analyze tables to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTables(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String[] attributeNames) {

    // Check each table
    boolean result = false;
    String contents = analysis.getContents();
    int index = contents.indexOf("{|", 0);
    while (index >= 0) {
      if ((index == 0) || (contents.charAt(index - 1) == '\n')) {
        int lineEnd = contents.indexOf('\n', index);
        if (lineEnd < 0) {
          lineEnd = contents.length();
        }
        String line = contents.substring(index, lineEnd);
        for (String attributeName : attributeNames) {
          int attributeIndex = line.indexOf(attributeName);
          if ((attributeIndex > 0) &&
              !Character.isLetterOrDigit(line.charAt(attributeIndex - 1))) {
            if (errors == null) {
              return true;
            }
            result = true;
            attributeIndex += index;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, attributeIndex, attributeIndex + attributeName.length());
            errors.add(errorResult);
          }
        }
      }
      index = contents.indexOf("{|", index + 2);
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String tagName,
      String[] attributeNames) {

    // Check each tag
    boolean result = false;
    List<PageElementTag> tags = analysis.getTags(tagName);
    for (PageElementTag tag : tags) {
      if ((tag != null) && (!tag.isEndTag())) {
        String tagText = analysis.getContents().substring(tag.getBeginIndex(), tag.getEndIndex());
        for (String attributeName : attributeNames) {
          int pos = tagText.indexOf(attributeName);
          if (pos > 0) {
            if (errors == null) {
              return true;
            }
            result = true;
            int beginIndex = tag.getBeginIndex() + pos;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, beginIndex + attributeName.length());
            errors.add(errorResult);
          }
        }
      }
    }
    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    //parameters.put("references_templates", GT._("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"));
    parameters.put("templates", GT._("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"));
    return parameters;
  }
}
