/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.contents.ContentsComment;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 85 of check wikipedia project.
 * Error 85: Tag without content
 */
public class CheckErrorAlgorithm085 extends CheckErrorAlgorithmBase {

  private final static String[] interestingTags = {
    PageElementTag.TAG_HTML_CENTER,
    PageElementTag.TAG_HTML_DIV,
    PageElementTag.TAG_HTML_SPAN,
    PageElementTag.TAG_HTML_SUP,
    PageElementTag.TAG_WIKI_INCLUDEONLY,
    PageElementTag.TAG_WIKI_GALLERY,
    PageElementTag.TAG_WIKI_NOINCLUDE,
  };

  private final static String[] ignoredTags = {
    PageElementTag.TAG_HTML_CODE,
    PageElementTag.TAG_WIKI_NOWIKI,
    PageElementTag.TAG_WIKI_PRE,
    PageElementTag.TAG_WIKI_SCORE,
  };

  public CheckErrorAlgorithm085() {
    super("Tag without content");
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

    // Check each tag
    List<PageElementTag> tags = analysis.getTags();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTag tag : tags) {
      if (!tag.isFullTag() && !tag.isEndTag() && tag.isComplete()) {

        // Check if tag can be of interest
        boolean interesting = false;
        for (String tagName : interestingTags) {
          if (tagName.equals(tag.getName())) {
            interesting = true;
          }
        }
  
        // Check tag
        if (interesting) {
          ErrorLevel errorLevel = ErrorLevel.ERROR;

          // Check if text is found inside the tag
          boolean textFound = false;
          boolean ignoredText = false;
          boolean isEmpty = true;
          int currentIndex = tag.getValueBeginIndex();
          int lastIndex = tag.getValueEndIndex();
          StringBuilder replacementText = new StringBuilder();
          replacementText.append(contents.substring(tag.getBeginIndex(), tag.getEndIndex()));
          boolean useReplacement = false;
          while (!textFound && (currentIndex < lastIndex)) {
            char currentChar = contents.charAt(currentIndex);
            if (Character.isWhitespace(currentChar) || (' ' == currentChar)) {
              replacementText.append(currentChar);
              currentIndex++;
            } else if (currentChar == '<') {
              isEmpty = false;
              boolean ok = false;
              if (!ok) {
                PageElementTag internalTag = analysis.isInTag(currentIndex);
                if (internalTag != null) {
                  for (String ignoredTag : ignoredTags) {
                    if (ignoredTag.equals(internalTag.getName())) {
                      ok = true;
                      ignoredText = true;
                      errorLevel = ErrorLevel.WARNING;
                      if (PageElementTag.TAG_WIKI_NOWIKI.equals(ignoredTag)) {
                        replacementText.append(contents.substring(
                            internalTag.getValueBeginIndex(), internalTag.getValueEndIndex()));
                        useReplacement = true;
                      } else {
                        replacementText.append(contents.substring(
                            currentIndex, internalTag.getCompleteEndIndex()));
                      }
                      currentIndex = internalTag.getCompleteEndIndex();
                    }
                  }
                } else {
                  ContentsComment comment = analysis.isInComment(currentIndex);
                  if (comment != null) {
                    ok = true;
                    replacementText.append(contents.substring(currentIndex, comment.getEndIndex()));
                    errorLevel = ErrorLevel.WARNING;
                    currentIndex = comment.getEndIndex();
                  }
                }
              }
              if (!ok) {
                textFound = true;
              }
            } else {
              textFound = true;
            }
          }
          boolean shouldReport = !textFound;
          replacementText.append(contents.substring(
              tag.getValueEndIndex(), tag.getCompleteEndIndex()));

          // Check if tag has arguments
          boolean hasUnsafeArguments = false;
          if (shouldReport) {
            if (tag.getParametersCount() > 0) {
              String tagName = tag.getName();
              if (PageElementTag.TAG_WIKI_REF.equals(tagName)) {
                shouldReport = false;
              } else if (PageElementTag.TAG_HTML_SPAN.equals(tagName)) {
                for (int paramNum = 0; paramNum < tag.getParametersCount(); paramNum++) {
                  Parameter param = tag.getParameter(paramNum);
                  if (param != null) {
                    String paramName = param.getName();
                    if (!"contenteditable".equals(paramName)) {
                      hasUnsafeArguments = true;
                    }
                  }
                }
              } else if (PageElementTag.TAG_HTML_DIV.equals(tagName)) {
                if ((tag.getParametersCount() == 1) && (tag.getParameter("id") != null)) {
                  shouldReport = false;
                }
                for (int paramNum = 0; paramNum < tag.getParametersCount(); paramNum++) {
                  hasUnsafeArguments = true;
                  Parameter param = tag.getParameter("style");
                  if (param != null) {
                    String paramValue = param.getValue();
                    if (paramValue != null) {
                      String[] styles = paramValue.split(";");
                      for (String style : styles) {
                        int colonIndex = style.indexOf(':');
                        if ((colonIndex > 0) &&
                            ("clear".equalsIgnoreCase(style.substring(0, colonIndex).trim()))) {
                          shouldReport = false;
                        }
                      }
                    }
                  }
                }
              } else {
                hasUnsafeArguments = true;
              }
            }
          }

          // Report error
          if (shouldReport) {
            if (errors == null) {
              return true;
            }
            result = true;

            // Define the extension of the replacement
            int beginIndex = tag.getCompleteBeginIndex();
            int endIndex = tag.getCompleteEndIndex();
            String internalValue = contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
            if (!ignoredText) {
              if ((endIndex == contents.length()) ||
                  (contents.charAt(endIndex) == '\n')) {
                int tmpBeginIndex = beginIndex;
                while ((tmpBeginIndex > 0) && (contents.charAt(tmpBeginIndex - 1) == '\n')) {
                  tmpBeginIndex--;
                }
                int tmpEndIndex = endIndex;
                while ((tmpEndIndex < contents.length()) && (contents.charAt(tmpEndIndex) == '\n')) {
                  tmpEndIndex++;
                }
                int countCR = (beginIndex - tmpBeginIndex) + (tmpEndIndex - endIndex);
                if (tmpBeginIndex == 0) {
                  countCR += 2;
                }
                if (tmpEndIndex == contents.length()) {
                  countCR++;
                }
                if (countCR > 1) {
                  while ((internalValue.length() > 0) &&
                         (Character.isWhitespace(internalValue.charAt(0)))) {
                    internalValue = internalValue.substring(1);
                  }
                  while ((internalValue.length() > 0) &&
                         (Character.isWhitespace(internalValue.charAt(internalValue.length() - 1)))) {
                    internalValue = internalValue.substring(0, internalValue.length() - 1);
                  }
                }
                /*if (countCR == 1) {
                  while ((internalValue.length() > 1) &&
                         (Character.isWhitespace(internalValue.charAt(0))) &&
                         (Character.isWhitespace(internalValue.charAt(1)))) {
                    internalValue = internalValue.substring(1);
                  }
                  while ((internalValue.length() > 1) &&
                         (Character.isWhitespace(internalValue.charAt(internalValue.length() - 1))) &&
                         (Character.isWhitespace(internalValue.charAt(internalValue.length() - 2)))) {
                    internalValue = internalValue.substring(0, internalValue.length() - 1);
                  }
                }*/
                if (countCR > 2) {
                  int delta = Math.min(countCR - 2, beginIndex - tmpBeginIndex);
                  beginIndex -= delta;
                  countCR -= delta;
                }
                if (countCR > 2) {
                  int delta = Math.min(countCR - 2, tmpEndIndex - endIndex);
                  endIndex += delta;
                }
              }
            }
            CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex, errorLevel);

            // Suggest replacements
            if (!ignoredText) {
              if (internalValue.length() > 0) {
                errorResult.addReplacement(internalValue, !hasUnsafeArguments && isEmpty);
                errorResult.addReplacement("");
              } else {
                errorResult.addReplacement("", !hasUnsafeArguments && isEmpty);
              }
            } else {
              if (useReplacement) {
                errorResult.addReplacement(replacementText.toString());
              }
              if (PageElementTag.TAG_HTML_CENTER.equals(tag.getName())) {
                for (String[] template : centerTemplates) {
                  if (template.length > 1) {
                    StringBuilder replacement = new StringBuilder();
                    replacement.append("{{");
                    replacement.append(template[0]);
                    replacement.append("|");
                    replacement.append(template[1]);
                    replacement.append("=");
                    replacement.append(contents.substring(
                        tag.getValueBeginIndex(), tag.getValueEndIndex()));
                    replacement.append("}}");
                    errorResult.addReplacement(
                        replacement.toString(),
                        GT._T("Use {0}", PageElementTemplate.createTemplate(template[0])));
                  }
                }
              }
            }
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
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates that can replace center tags */
  private static final String PARAMETER_CENTER_TEMPLATES = "center_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_CENTER_TEMPLATES, true, true, false);
    centerTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        centerTemplates.addAll(tmpList);
      }
    }
  }

  /** Templates that can replace center tags */
  private final List<String[]> centerTemplates = new ArrayList<>();

  /**
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        PARAMETER_CENTER_TEMPLATES,
        GT._T("A list of templates that can be used to replace &lt;center&gt; tags"));
    return parameters;
  }
}
