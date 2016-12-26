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
    "data-cx-state",
    "data-cx-weight",
    "data-source",
    "id",
    "-moz-border-radius-bottomleft",
    "-moz-border-radius-bottomright",
    "-moz-border-radius-topleft",
    "-moz-border-radius-topright",
    "-moz-border-radius",
  };

  static {
    TAGS.put(null, new String[] {
        "contenteditable",
        "data-cx-draft",
        "date-cx-state",
        "data-cx-weight",
        "data-source",
        "-moz-border-radius-bottomleft",
        "-moz-border-radius-bottomright",
        "-moz-border-radius-topleft",
        "-moz-border-radius-topright",
        "-moz-border-radius",
        "-moz-box-shadow",
        "-moz-linear-gradient",
        "-webkit-border-radius",
    });
    TAGS.put(PageElementTag.TAG_HTML_CENTER, new String[] {
        "id",
    });
    TAGS.put(PageElementTag.TAG_HTML_DIV, new String[] {
        "-moz-column-count",
        "-moz-column-gap",
        "-moz-column-width",
        "-webkit-column-count",
        "-webkit-column-width",
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

        // Analyze general table attributes
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
            int beginIndex = attributeIndex;
            int endIndex = beginIndex + attributeName.length();
            while ((beginIndex > 0) && (line.charAt(beginIndex - 1) == ' ')) {
              beginIndex--;
            }
            boolean delete = false;
            if (endIndex < line.length()) {
              int tmpIndex = endIndex;
              if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '=')) {
                tmpIndex++;
                if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '"')) {
                  tmpIndex++;
                  while ((tmpIndex < line.length()) && (line.charAt(tmpIndex) != '"')) {
                    tmpIndex++;
                  }
                  if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '"')) {
                    tmpIndex++;
                    endIndex = tmpIndex;
                    delete = true;
                  }
                }
              }
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, index + beginIndex, index + endIndex);
            if (delete) {
              errorResult.addReplacement("");
            }
            errors.add(errorResult);
          }
        }

        // Analyze table details
        int lineBegin = lineEnd + 1;
        boolean tableEndFound = false;
        while ((lineBegin < contents.length()) && !tableEndFound) {
          lineEnd = contents.indexOf('\n', lineBegin);
          if (lineEnd < 0) {
            lineEnd = contents.length();
          }
          line = contents.substring(lineBegin, lineEnd);

          if (line.indexOf("|}") >= 0) {
            tableEndFound = true;
          } else {
            if (line.startsWith("|-")) {

              // Analyze new table lines
              int attributeIndex = line.indexOf("id");
              boolean shouldReport = (attributeIndex > 0);
              if (shouldReport &&
                  Character.isLetterOrDigit(line.charAt(attributeIndex - 1))) {
                shouldReport = false;
              }
              if (shouldReport &&
                  (attributeIndex + 2 >= line.length()) ||
                  Character.isLetterOrDigit(line.charAt(attributeIndex + 2))) {
                shouldReport = false;
              }
              int beginIndex = attributeIndex;
              int endIndex = beginIndex + 2;
              boolean delete = false;
              if (shouldReport) {
                while ((beginIndex > 0) && (line.charAt(beginIndex - 1) == ' ')) {
                  beginIndex--;
                }
                if (endIndex < line.length()) {
                  int tmpIndex = endIndex;
                  if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '=')) {
                    tmpIndex++;
                    if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '"')) {
                      tmpIndex++;
                      while ((tmpIndex < line.length()) && Character.isDigit(line.charAt(tmpIndex))) {
                        tmpIndex++;
                      }
                      if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '"')) {
                        tmpIndex++;
                        endIndex = tmpIndex;
                        delete = true;
                      }
                    }
                  }
                }
                if (!delete) {
                  shouldReport = false;
                }
              }

              // Report error
              if (shouldReport) {
                if (errors == null) {
                  return true;
                }
                result = true;
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, lineBegin + beginIndex, lineBegin + endIndex);
                if (delete) {
                  errorResult.addReplacement("");
                }
                errors.add(errorResult);
              }
            } else if (line.startsWith("!") || line.startsWith("|")) {

              // Analyze table lines
              char separator = line.charAt(0);
              int attributeIndex = line.indexOf("id");
              while (attributeIndex > 0) {
                boolean shouldReport = true;
                if (shouldReport &&
                    Character.isLetterOrDigit(line.charAt(attributeIndex - 1))) {
                  shouldReport = false;
                }
                if (shouldReport &&
                    ((attributeIndex + 2 >= line.length()) ||
                     Character.isLetterOrDigit(line.charAt(attributeIndex + 2)))) {
                  shouldReport = false;
                }
                if (shouldReport &&
                    (analysis.isInExternalLink(lineBegin + attributeIndex) != null)) {
                  shouldReport = false;
                }
                if (shouldReport &&
                    (analysis.isInTemplate(lineBegin + attributeIndex) != null)) {
                  shouldReport = false;
                }
                int beginIndex = attributeIndex;
                int endIndex = beginIndex + 2;
                String replacement = null;
                if (shouldReport) {
                  while ((beginIndex > 0) && (line.charAt(beginIndex - 1) == ' ')) {
                    beginIndex--;
                  }
                  if (endIndex < line.length()) {
                    int tmpIndex = endIndex;
                    if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '=')) {
                      tmpIndex++;
                      if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '"')) {
                        tmpIndex++;
                        while ((tmpIndex < line.length()) && Character.isDigit(line.charAt(tmpIndex))) {
                          tmpIndex++;
                        }
                        if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '"')) {
                          tmpIndex++;
                          endIndex = tmpIndex;
                          replacement = "";
                          if ((beginIndex > 0) && (line.charAt(beginIndex - 1) == separator)) {
                            while ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == ' ')) {
                              tmpIndex++;
                            }
                            if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) == '|')) {
                              tmpIndex++;
                              endIndex = tmpIndex;
                              if ((tmpIndex < line.length()) && (line.charAt(tmpIndex) != ' ')) {
                                if (line.charAt(beginIndex) == ' ') {
                                  beginIndex++;
                                } else {
                                  replacement = " ";
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    if (replacement == null) {
                      shouldReport = false;
                    }
                  }
                }

                // Report error
                if (shouldReport) {
                  if (errors == null) {
                    return true;
                  }
                  result = true;
                  CheckErrorResult errorResult = createCheckErrorResult(
                      analysis, lineBegin + beginIndex, lineBegin + endIndex);
                  if (replacement != null) {
                    errorResult.addReplacement("");
                  }
                  errors.add(errorResult);
                }
                attributeIndex = line.indexOf("id", attributeIndex + 2);
              }
            }
          }

          lineBegin = lineEnd + 1;
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
    List<PageElementTag> tags = (tagName != null) ? analysis.getTags(tagName) : analysis.getTags();
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
