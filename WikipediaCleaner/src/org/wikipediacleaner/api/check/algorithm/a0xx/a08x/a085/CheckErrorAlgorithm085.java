/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a085;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 85 of check wikipedia project.
 * Error 85: Tag without content
 */
public class CheckErrorAlgorithm085 extends CheckErrorAlgorithmBase {

  private final static Set<TagType> interestingTags = new HashSet<>();

  private final static Set<TagType> ignoredTags = new HashSet<>();

  private final static Set<TagType> unsafeTags = new HashSet<>();

  private final static String HTML_SPACE = "&#x20;";

  static {
    interestingTags.add(HtmlTagType.CENTER);
    interestingTags.add(HtmlTagType.DIV);
    interestingTags.add(HtmlTagType.SPAN);
    interestingTags.add(HtmlTagType.SUP);
    interestingTags.add(WikiTagType.GALLERY);
    interestingTags.add(WikiTagType.INCLUDEONLY);
    interestingTags.add(WikiTagType.NOINCLUDE);

    unsafeTags.add(WikiTagType.INCLUDEONLY);
    unsafeTags.add(WikiTagType.NOINCLUDE);

    // ignoredTags.add(HtmlTagType.CODE);
    // ignoredTags.add(WikiTagType.NOWIKI);
    // ignoredTags.add(WikiTagType.PRE);
    // ignoredTags.add(WikiTagType.SCORE);
  }

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
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, tag, errors);
    }
    return result;
  }

  public boolean analyzeTag(
      PageAnalysis analysis, PageElementTag tag,
      Collection<CheckErrorResult> errors) {

    // Check if tag can be of interest
    if (tag.isFullTag() || tag.isEndTag() || !tag.isComplete()) {
      return false;
    }
    if (!interestingTags.contains(tag.getType())) {
      return false;
    }
    if (analysis.getSurroundingTag(WikiTagType.NOWIKI, tag.getBeginIndex()) != null) {
      return false;
    }

    String contents = analysis.getContents();
    ErrorLevel errorLevel = ErrorLevel.ERROR;

    // Check if text is found inside the tag
    boolean ignoredText = false;
    boolean isEmpty = true;
    int currentIndex = tag.getValueBeginIndex();
    int lastIndex = tag.getValueEndIndex();
    StringBuilder replacementText = new StringBuilder();
    replacementText.append(contents.substring(tag.getBeginIndex(), tag.getEndIndex()));
    boolean useReplacement = false;
    while (currentIndex < lastIndex) {
      char currentChar = contents.charAt(currentIndex);
      if (Character.isWhitespace(currentChar) || (' ' == currentChar)) {
        replacementText.append(currentChar);
        currentIndex++;
      } else if ((currentChar == '&') &&
          contents.startsWith(HTML_SPACE, currentIndex)) {
        replacementText.append(HTML_SPACE);
        currentIndex += HTML_SPACE.length();
      } else if (currentChar == '<') {
        isEmpty = false;
        PageElementTag internalTag = analysis.isInTag(currentIndex);
        if (internalTag != null) {
          if (!ignoredTags.contains(internalTag.getType())) {
            return false;
          }
          ignoredText = true;
          errorLevel = ErrorLevel.WARNING;
          if (WikiTagType.NOWIKI.equals(internalTag.getType())) {
            replacementText.append(contents.substring(
                internalTag.getValueBeginIndex(), internalTag.getValueEndIndex()));
            useReplacement = true;
          } else {
            replacementText.append(contents.substring(
                currentIndex, internalTag.getCompleteEndIndex()));
          }
          currentIndex = internalTag.getCompleteEndIndex();
        } else {
          ContentsComment comment = analysis.comments().getAt(currentIndex);
          if (comment == null) {
            return false;
          }
          replacementText.append(contents.substring(currentIndex, comment.getEndIndex()));
          errorLevel = ErrorLevel.WARNING;
          currentIndex = comment.getEndIndex();
        }
      } else {
        return false;
      }
    }
    replacementText.append(contents.substring(
        tag.getValueEndIndex(), tag.getCompleteEndIndex()));

    // Check if tag has arguments
    boolean hasUnsafeArguments = false;
    if (tag.getParametersCount() > 0) {
      if (WikiTagType.REF.equals(tag.getType())) {
        return false;
      } else if (HtmlTagType.SPAN.equals(tag.getType())) {
        for (int paramNum = 0; paramNum < tag.getParametersCount(); paramNum++) {
          Parameter param = tag.getParameter(paramNum);
          if (param != null) {
            String paramName = param.getName();
            if (!"contenteditable".equals(paramName)) {
              hasUnsafeArguments = true;
            }
          }
        }
      } else if (HtmlTagType.DIV.equals(tag.getType())) {
        if ((tag.getParametersCount() == 1) && (tag.getParameter("id") != null)) {
          return false;
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
                  return false;
                }
              }
            }
          }
        }
      } else {
        hasUnsafeArguments = true;
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }

    // Check if special characters are around the tag
    int beginIndex = tag.getCompleteBeginIndex();
    int endIndex = tag.getCompleteEndIndex();
    boolean isBetweenSpecialCharacters = false;
    if ((beginIndex > 0) && (endIndex < contents.length() - 1)) {
      char previousChar = contents.charAt(beginIndex - 1);
      char nextChar = contents.charAt(endIndex);
      if ((previousChar == nextChar) && ("{}[]".indexOf(previousChar) >= 0)) {
        isBetweenSpecialCharacters = true;
      }
    }

    // Check if tag is unsafe
    boolean unsafeTag = unsafeTags.contains(tag.getType());

    // Define the extension of the replacement
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
        errorResult.addReplacement(internalValue, !hasUnsafeArguments && isEmpty && !isBetweenSpecialCharacters && !unsafeTag);
        errorResult.addReplacement("");
      } else {
        errorResult.addReplacement("", !hasUnsafeArguments && isEmpty && !isBetweenSpecialCharacters && !unsafeTag);
      }
    } else {
      if (useReplacement) {
        errorResult.addReplacement(replacementText.toString());
      }
      if (HtmlTagType.CENTER.equals(tag.getType())) {
        for (String[] template : centerTemplates) {
          if (template.length > 1) {
            TemplateBuilder builder = TemplateBuilder.from(template[0]);
            builder.addParam(
                template[1],
                contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex()));
            errorResult.addReplacement(
                builder.toString(),
                GT._T("Use {0}", TemplateBuilder.from(template[0]).toString()));
          }
        }
      }
    }
    errors.add(errorResult);

    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isInMainNamespace()) {
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
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_CENTER_TEMPLATES,
        GT._T("A list of templates that can be used to replace &lt;center&gt; tags"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Template that can be used to replace &lt;center&gt; tags")),
            new AlgorithmParameterElement(
                "parameter name",
                GT._T("Name of the parameter to use"),
                true)
        },
        true));
  }
}
