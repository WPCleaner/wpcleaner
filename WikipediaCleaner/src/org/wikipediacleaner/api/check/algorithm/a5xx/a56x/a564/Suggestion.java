/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a564;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;

/**
 * Suggestion for a replacement.
 */
class Suggestion {

  @Nonnull final String replacement;

  @Nullable final String paramName;

  final boolean automatic;

  private Suggestion(@Nonnull String replacement, @Nullable String paramName, boolean automatic) {
    this.replacement = replacement;
    this.paramName = paramName;
    this.automatic = automatic;
  }

  /**
   * Create a suggestion to delete a parameter.
   * 
   * @param contents Page contents.
   * @param templateParam Parameter to be commented.
   * @param automatic True if the deletion is automatic.
   * @return Suggestion.
   */
  public static Suggestion deleteParam(
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
          return new Suggestion(contents.substring(beginIndex + crIndex, endIndex), null, automatic);
        }
      }
    }
    return new Suggestion(StringUtils.EMPTY, null, automatic);
  }

  /**
   * Create a suggestion to comment a parameter.
   * 
   * @param contents Page contents.
   * @param templateParam Parameter to be commented.
   * @param automatic True if the comment is automatic.
   * @return Suggestion.
   */
  public static Suggestion commentParam(
      String contents,
      PageElementTemplate.Parameter templateParam,
      boolean automatic) {
    String fullText = contents.substring(templateParam.getBeginIndex(), templateParam.getEndIndex());
    return new Suggestion(CommentBuilder.from(fullText).toString(), null, automatic);
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
  public static Suggestion replaceParam(
      String contents,
      PageElementTemplate.Parameter templateParam,
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
          contents.substring(beginIndex, nameStartIndex) +
          newName +
          contents.substring(nameStartIndex + templateParam.getName().length(), valueStartIndex) +
          newValue +
          contents.substring(valueStartIndex + templateParam.getValue().length(), endIndex);
      return new Suggestion(newText, newName, automatic);
    }

    // Implicit parameter name
    String newText =
        contents.substring(beginIndex, valueStartIndex) +
        newName +
        "=" +
        newValue +
        contents.substring(valueStartIndex + templateParam.getValue().length(), endIndex);
    return new Suggestion(newText, newName, automatic);
  }
}
