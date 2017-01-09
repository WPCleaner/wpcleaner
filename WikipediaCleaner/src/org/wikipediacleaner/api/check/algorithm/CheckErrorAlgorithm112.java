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
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
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
  private final static Map<String, String[]> TAG_ATTRIBUTES = new HashMap<>();

  /** List of styles for each kind of tag */
  private final static Map<String, String[]> TAG_STYLES = new HashMap<>();

  /** List of attributes for tables */
  private final static String[] TABLE_ATTRIBUTES = new String[] {
    "contenteditable",
    "data-cx-draft",
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

  // Initialization of lists of attributes/styles/...
  static {
    TAG_ATTRIBUTES.put(null, new String[] {
        "contenteditable",
        "data-cx-draft",
        "data-cx-state",
        "data-cx-weight",
        "data-source",
    });
    TAG_STYLES.put(null, new String[] {
        "-moz-border-radius-bottomleft",
        "-moz-border-radius-bottomright",
        "-moz-border-radius-topleft",
        "-moz-border-radius-topright",
        "-moz-border-radius",
        "-moz-box-shadow",
        "-moz-linear-gradient",
        "-webkit-border-radius",
    });

    TAG_ATTRIBUTES.put(PageElementTag.TAG_HTML_CENTER, new String[] {
        "id",
    });

    TAG_STYLES.put(PageElementTag.TAG_HTML_DIV, new String[] {
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
    for (Entry<String, String[]> tag : TAG_ATTRIBUTES.entrySet()) {
      result |= analyzeTagAttributes(analysis, errors, tag.getKey(), tag.getValue());
      if (result && (errors == null)) {
        return result;
      }
    }
    for (Entry<String, String[]> tag : TAG_STYLES.entrySet()) {
      result |= analyzeTagStyles(analysis, errors, tag.getKey(), tag.getValue());
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
            int prefix = 0;
            while ((prefix < line.length()) &&
                   (line.charAt(prefix) == ' ')) {
              prefix++;
            }
            if (line.startsWith("|-", prefix)) {

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
            } else if (line.startsWith("!", prefix) || line.startsWith("|", prefix)) {

              // Analyze table lines
              char separator = line.charAt(prefix);
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
                          if ((beginIndex > 0) &&
                              ((line.charAt(beginIndex - 1) == separator)||
                               (line.charAt(beginIndex - 1) == '|'))) {
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
   * Analyze a page to check if errors are present in tag attributes.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagName Tag name.
   * @param attributeName Tag attribute names.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTagAttributes(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String tagName,
      String[] attributeNames) {

    // Check each tag
    boolean result = false;
    String contents = analysis.getContents();
    List<PageElementTag> tags = (tagName != null) ? analysis.getTags(tagName) : analysis.getTags();
    for (PageElementTag tag : tags) {
      if ((tag != null) && (!tag.isEndTag())) {
        for (int paramNum = 0; paramNum < tag.getParametersCount(); paramNum++) {
          Parameter param = tag.getParameter(paramNum);
          for (String attributeName : attributeNames) {
            if (attributeName.equals(param.getName())) {
              if (errors == null) {
                return true;
              }
              result = true;
              int beginIndex = tag.getBeginIndex() + param.getOffsetBegin();
              while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == ' ')) {
                beginIndex--;
              }
              int endIndex = tag.getBeginIndex() + param.getOffsetEnd();
              if ((endIndex < contents.length()) &&
                  (contents.charAt(endIndex) != ' ') &&
                  (contents.charAt(endIndex) != '>') &&
                  (contents.charAt(beginIndex) == ' ')) {
                beginIndex++;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, beginIndex, endIndex);
              errorResult.addReplacement("");
              errors.add(errorResult);
            }
          }
        }
      }
    }
    return result;
  }

  /**
   * Analyze a page to check if errors are present in tag styles.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagName Tag name.
   * @param styleNames Tag style names.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTagStyles(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String tagName,
      String[] styleNames) {

    // Check each tag
    boolean result = false;
    String contents = analysis.getContents();
    List<PageElementTag> tags = (tagName != null) ? analysis.getTags(tagName) : analysis.getTags();
    for (PageElementTag tag : tags) {
      if ((tag != null) && (!tag.isEndTag())) {
        Parameter styleParam = tag.getParameter("style");
        if ((styleParam != null) && (styleParam.getValue() != null)) {
          String styleValue = styleParam.getValue();
          for (String styleName : styleNames) {
            int offset = styleValue.indexOf(styleName);
            boolean found = false;
            if (offset == 0) {
              found = true;
            } else if ((offset > 0) &&
                       (" ;".indexOf(styleValue.charAt(offset - 1)) >= 0)) {
              found = true;
            }
            if (found) {
              int end = offset + styleName.length();
              if ((end >= styleValue.length()) ||
                  (styleValue.charAt(end) == ':')) {

                // Report error
                if (errors == null) {
                  return true;
                }
                result = true;
                boolean reported = false;

                // Specific treatment for column count
                if ("-moz-column-count".equals(styleName) ||
                    "-webkit-column-count".equals(styleName)) {
                  int currentIndex = 0;
                  boolean onlyColumnCount = true;
                  int columnCount = -1;
                  while (onlyColumnCount && (currentIndex < styleValue.length())) {
                    int tmpIndex = currentIndex;
                    while ((tmpIndex < styleValue.length()) &&
                           (" :".indexOf(styleValue.charAt(tmpIndex)) < 0)) {
                      tmpIndex++;
                    }
                    if ((tmpIndex > currentIndex) &&
                        (tmpIndex < styleValue.length()) &&
                        (styleValue.charAt(tmpIndex) == ':')) {
                      String tmpStyleName = styleValue.substring(currentIndex, tmpIndex);
                      tmpIndex++;
                      currentIndex = tmpIndex;
                      while ((tmpIndex < styleValue.length()) &&
                             (" ;".indexOf(styleValue.charAt(tmpIndex)) < 0)) {
                        tmpIndex++;
                      }
                      if (tmpIndex > currentIndex) {
                        String tmpStyleValue = styleValue.substring(currentIndex, tmpIndex);
                        if ("column-count".equals(tmpStyleName) ||
                            "-moz-column-count".equals(tmpStyleName) ||
                            "-webkit-column-count".equals(tmpStyleName)) {
                          try {
                            int tmpColumnCount = Integer.parseInt(tmpStyleValue);
                            if (tmpColumnCount <= 0) {
                              onlyColumnCount = false;
                            } else if ((columnCount < 0) || (columnCount == tmpColumnCount)) {
                              columnCount = tmpColumnCount;
                            } else {
                              onlyColumnCount = false;
                            }
                          } catch (NumberFormatException e) {
                            onlyColumnCount = false;
                          }
                        } else if ("-moz-column-gap".equals(tmpStyleName) ||
                                   "-moz-column-width".equals(tmpStyleName) ||
                                   "-webkit-column-width".equals(tmpStyleName)) {
                          // Nothing to do
                        } else {
                          onlyColumnCount = false;
                        }
                      }
                      while ((tmpIndex < styleValue.length()) &&
                             (" ;".indexOf(styleValue.charAt(tmpIndex)) >= 0)) {
                        tmpIndex++;
                      }
                      currentIndex = tmpIndex;
                    } else {
                      onlyColumnCount = false;
                    }
                  }

                  if (onlyColumnCount && (columnCount > 0)) {
                    List<String[]> columnsTemplates = analysis.getWPCConfiguration().getStringArrayList(
                        WPCConfigurationStringList.COLUMNS_TEMPLATES);
                    List<String[]> columns2Templates = analysis.getWPCConfiguration().getStringArrayList(
                        WPCConfigurationStringList.COLUMNS2_TEMPLATES);

                    if (((columnsTemplates != null) && (!columnsTemplates.isEmpty())) ||
                        ((columns2Templates != null) && (!columns2Templates.isEmpty()))) {
                      CheckErrorResult errorResult = createCheckErrorResult(
                          analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
                      String inside = contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
                      StringBuilder replacement = new StringBuilder();
                      if (columns2Templates != null) {
                        for (String[] columns2Template : columns2Templates) {
                          if (columns2Template.length > 2) {
                            replacement.setLength(0);
                            replacement.append("{{");
                            replacement.append(columns2Template[0]);
                            replacement.append("|");
                            replacement.append(columns2Template[2]);
                            replacement.append("=");
                            replacement.append(columnCount);
                            replacement.append("}}");
                            replacement.append(inside);
                            replacement.append(PageElementTemplate.createTemplate(columns2Template[1]));
                            errorResult.addReplacement(
                                replacement.toString(),
                                GT._("Use {0} and {1}", new Object[] {
                                    PageElementTemplate.createTemplate(columns2Template[0]),
                                    PageElementTemplate.createTemplate(columns2Template[1]) }));
                          }
                        }
                      }
                      if (columnsTemplates != null) {
                        for (String[] columnsTemplate : columnsTemplates) {
                          if (columnsTemplate.length > 2) {
                            replacement.setLength(0);
                            replacement.append("{{");
                            replacement.append(columnsTemplate[0]);
                            replacement.append("|");
                            replacement.append(columnsTemplate[2]);
                            replacement.append("=");
                            replacement.append(columnCount);
                            replacement.append("|");
                            replacement.append(columnsTemplate[1]);
                            replacement.append("=");
                            replacement.append(inside);
                            replacement.append("}}");
                            errorResult.addReplacement(
                                replacement.toString(),
                                GT._("Use {0}", PageElementTemplate.createTemplate(columnsTemplate[0])));
                          }
                        }
                      }
                      errors.add(errorResult);
                      reported = true;
                    }
                  }
                }

                // General case
                if (!reported) {
                  int beginIndex = tag.getBeginIndex() + styleParam.getOffsetValue() + offset;
                  int endIndex = beginIndex + styleName.length();
                  while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == ' ')) {
                    beginIndex--;
                  }
                  CheckErrorResult errorResult = createCheckErrorResult(
                      analysis, beginIndex, endIndex);
                  errorResult.addReplacement("");
                  errors.add(errorResult);
                  reported = true;
                }
              }
            }
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
