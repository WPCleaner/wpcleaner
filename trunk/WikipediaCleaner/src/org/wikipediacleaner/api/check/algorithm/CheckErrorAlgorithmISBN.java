/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.i18n.GT;


/**
 * Base class for errors on ISBN numbers.
 */
public abstract class CheckErrorAlgorithmISBN extends CheckErrorAlgorithmBase {

  /**
   * @param name Algorithm name.
   */
  protected CheckErrorAlgorithmISBN(String name) {
    super(name);
  }

  /**
   * @param analysis Page analysis.
   * @param isbn ISBN.
   * @param checkForComment True to check for a comment after the ISBN.
   * @return Error result.
   */
  protected CheckErrorResult createCheckErrorResult(
      PageAnalysis analysis, PageElementISBN isbn,
      boolean checkForComment) {
    ErrorLevel level = ErrorLevel.ERROR;
    if (checkForComment) {
      String contents = analysis.getContents();
      int index = isbn.getEndIndex();
      while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
        index++;
      }
      if ((index < contents.length()) && (contents.charAt(index) == '<')) {
        PageElementComment comment = analysis.isInComment(index);
        if (comment != null) {
          level = ErrorLevel.WARNING;
        }
      }
    }
    CheckErrorResult result = createCheckErrorResult(
        analysis.getPage(), isbn.getBeginIndex(), isbn.getEndIndex(), level);
    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param isbn ISBN.
   * @param reason Reason for the error.
   */
  protected void addHelpNeededTemplates(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISBN isbn, String reason) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> helpNeededTemplates = config.getStringArrayList(
        WPCConfigurationStringList.ISBN_HELP_NEEDED_TEMPLATES);
    if ((helpNeededTemplates != null) &&
        (!helpNeededTemplates.isEmpty())) {
      for (String[] helpNeededTemplate : helpNeededTemplates) {
        String replacement = isbn.askForHelp(helpNeededTemplate, reason);
        if (replacement != null) {
          errorResult.addReplacement(
              replacement.toString(),
              GT._("Ask for help using {0}", "{{" + helpNeededTemplate[0] + "}}"));
        }
      }
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param isbn ISBN.
   * @param reason Reason for the error.
   */
  protected void addHelpNeededComment(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISBN isbn, String reason) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    String helpNeededComment = config.getString(
        WPCConfigurationString.ISBN_HELP_NEEDED_COMMENT);
    if (helpNeededComment != null) {
      String replacement = isbn.askForHelp(helpNeededComment, reason);
      if (replacement != null) {
        String contents = analysis.getContents();
        replacement =
            contents.substring(isbn.getBeginIndex(), isbn.getEndIndex()) +
            replacement;
        errorResult.addReplacement(replacement, GT._("Add a comment"));
      }
    }
  }
}
