/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;

/**
 * Suggestion for a replacement of a template parameter.
 */
public class TemplateParameterSuggestion {

  @Nonnull private final String replacement;

  @Nullable private final String paramName;

  private final boolean automatic;

  private TemplateParameterSuggestion(@Nonnull String replacement, @Nullable String paramName, boolean automatic) {
    this.replacement = replacement;
    this.paramName = paramName;
    this.automatic = automatic;
  }

  public String getReplacement() {
    return replacement;
  }

  public String getParamName() {
    return paramName;
  }

  public boolean isAutomatic() {
    return automatic;
  }

  /**
   * Create a suggestion to delete a parameter.
   * 
   * @param contents Page contents.
   * @param templateParam Parameter to be commented.
   * @param automatic True if the deletion is automatic.
   * @return Suggestion.
   */
  public static TemplateParameterSuggestion deleteParam(
      String contents,
      PageElementTemplate.Parameter templateParam,
      boolean automatic) {
    int beginIndex = templateParam.getBeginIndex();
    int endIndex = templateParam.getEndIndex();
    int crIndex = contents.substring(beginIndex, endIndex).lastIndexOf('\n');
    if (crIndex >= 0) {
      int equalIndex = contents.substring(beginIndex, endIndex).lastIndexOf('=');
      if (equalIndex < crIndex) {
        int tmpIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, templateParam.getBeginIndex() - 1, " ");
        if ((tmpIndex < 0) || (contents.charAt(tmpIndex) != '\n')) {
          return new TemplateParameterSuggestion(contents.substring(beginIndex + crIndex, endIndex), null, automatic);
        }
      }
    }
    return new TemplateParameterSuggestion(StringUtils.EMPTY, null, automatic);
  }

  /**
   * Create a suggestion to comment a parameter.
   * 
   * @param analysis Page analysis.
   * @param templateParam Parameter to be commented.
   * @param automatic True if the comment is automatic.
   * @return Suggestion.
   */
  public static TemplateParameterSuggestion commentParam(
      PageAnalysis analysis,
      PageElementTemplate.Parameter templateParam,
      boolean automatic) {
    String fullText = analysis.getContents().substring(
        templateParam.getBeginIndex(),
        templateParam.getEndIndex());
    int firstChar = 0;
    while ((firstChar < fullText.length()) &&
           (" \n".indexOf(fullText.charAt(firstChar)) >= 0)) {
      firstChar++;
    }
    int lastChar = fullText.length();
    boolean lastCharFinished = false;
    while (!lastCharFinished) {
      lastCharFinished = true;
      while ((lastChar > firstChar) &&
             (" \n".indexOf(fullText.charAt(lastChar - 1)) >= 0)) {
        lastCharFinished = false;
        lastChar--;
      }
      if ((lastChar > firstChar) && (fullText.charAt(lastChar - 1) == '>')) {
        ContentsComment comment = analysis.comments().getEndsAt(templateParam.getBeginIndex() + lastChar);
        if (comment != null) {
          lastChar = comment.getBeginIndex() - templateParam.getBeginIndex();
        }
      }
    }
    String newText =
        fullText.substring(0, firstChar) +
        CommentBuilder.from(fullText.substring(firstChar, lastChar)).withWhitespace(false) +
        (lastChar < fullText.length() ? fullText.substring(lastChar) : StringUtils.EMPTY);
    return new TemplateParameterSuggestion(newText, null, automatic);
  }

  /**
   * Create a suggestion to replace a parameter.
   *
   * @param contents Page contents.
   * @param templateParam Parameter to be replaced.
   * @param newName New parameter name.
   * @param newValue New parameter value.
   * @param automatic True if the replacement is automatic.
   * @return Suggestion.
   */
  public static TemplateParameterSuggestion replaceParam(
          String contents,
          PageElementTemplate.Parameter templateParam,
          String newName,
          String newValue,
          boolean automatic) {
    return replaceParam(contents, templateParam, "", newName, newValue, automatic);
  }

  /**
   * Create a suggestion to replace a parameter.
   * 
   * @param contents Page contents.
   * @param templateParam Parameter to be replaced.
   * @param prefix Prefix before parameter
   * @param newName New parameter name.
   * @param newValue New parameter value.
   * @param automatic True if the replacement is automatic.
   * @return Suggestion.
   */
  public static TemplateParameterSuggestion replaceParam(
      String contents,
      PageElementTemplate.Parameter templateParam,
      String prefix,
      String newName,
      String newValue,
      boolean automatic) {
    int beginIndex = templateParam.getBeginIndex();
    int valueStartIndex = templateParam.getValueStartIndex();
    int endIndex = templateParam.getEndIndex();

    // Explicit parameter name
    if (StringUtils.equals(templateParam.getName(), templateParam.getComputedName())) {
      int nameStartIndex = templateParam.getNameStartIndex();
      String newText =
          prefix +
          contents.substring(beginIndex, nameStartIndex) +
          newName +
          contents.substring(nameStartIndex + templateParam.getName().length(), valueStartIndex) +
          newValue +
          contents.substring(valueStartIndex + templateParam.getValue().length(), endIndex);
      return new TemplateParameterSuggestion(newText, newName, automatic);
    }

    // Implicit parameter name
    String newText =
        contents.substring(beginIndex, valueStartIndex) +
        newName +
        "=" +
        newValue +
        contents.substring(valueStartIndex + templateParam.getValue().length(), endIndex);
    return new TemplateParameterSuggestion(newText, newName, automatic);
  }

  /**
   * Create a suggestion to replace a parameter (or delete it if it already exists).
   * 
   * @param contents Page contents.
   * @param template Template.
   * @param templateParam Parameter to be replaced.
   * @param newName New parameter name.
   * @param newValue New parameter value.
   * @param automaticReplacement True if the replacement can be automatic.
   * @param automaticDeletion True if the deletion can be automatic.
   * @return Suggestion.
   */
  public static TemplateParameterSuggestion replaceOrDeleteParam(
      String contents,
      PageElementTemplate template,
      PageElementTemplate.Parameter templateParam,
      String newName,
      String newValue,
      boolean automaticReplacement,
      boolean automaticDeletion) {

    // Simple replacement if new parameter doesn't already exist
    int otherParamIndex = template.getParameterIndex(newName);
    PageElementTemplate.Parameter otherParam = (otherParamIndex < 0) ? null : template.getParameter(otherParamIndex);
    if ((otherParam == null) ||
        StringUtils.equals(newName, templateParam.getComputedName())) {
      return replaceParam(contents, templateParam, newName, newValue, automaticReplacement);
    }

    // Check if the values are identical
    automaticDeletion &= StringUtils.equals(newValue, otherParam.getValue());
    return deleteParam(contents, templateParam, automaticDeletion);
  }

  public static TemplateParameterSuggestion splitParam(
      String contents,
      PageElementTemplate template,
      PageElementTemplate.Parameter templateParam,
      int splitIndex,
      String otherName,
      boolean automaticReplacement,
      boolean automaticDeletion) {

    // Simple trimming if new parameter already exists with the correct value
    int otherParamIndex = template.getParameterIndex(otherName);
    PageElementTemplate.Parameter otherParam = (otherParamIndex < 0) ? null : template.getParameter(otherParamIndex);
    if (otherParam != null) {
      String otherValue = contents.substring(splitIndex, templateParam.getEndIndex());
      if (StringUtils.equals(otherParam.getValue(), otherValue)) {
        String replacement = contents.substring(templateParam.getBeginIndex(), splitIndex);
        return new TemplateParameterSuggestion(replacement, otherName, automaticDeletion);
      }
    }

    // Do nothing if new parameter already exists
    if (otherParam != null) {
      String replacement = contents.substring(templateParam.getBeginIndex(), templateParam.getEndIndex());
      return new TemplateParameterSuggestion(replacement, templateParam.getComputedName(), false);
    }

    // Split parameter
    int beginIndex = templateParam.getBeginIndex();
    int endIndex = templateParam.getEndIndex();
    String newText =
        contents.substring(beginIndex, splitIndex) +
        " | " +
        otherName +
        " = " +
        contents.substring(splitIndex, endIndex);
    return new TemplateParameterSuggestion(newText, otherName, automaticReplacement);
  }
}
