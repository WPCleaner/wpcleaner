/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 70 of check wikipedia project.
 * Error 70: ISBN wrong length
 */
public class CheckErrorAlgorithm070 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm070() {
    super("ISBN wrong length");
  }

  /**
   * @param analysis Page analysis. 
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.PageAnalysis, java.util.Collection)
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> helpNeededTemplates = config.getStringArrayList(
        WPCConfigurationStringList.ISBN_HELP_NEEDED_TEMPLATES);
    String helpNeededComment = config.getString(
        WPCConfigurationString.ISBN_HELP_NEEDED_COMMENT);
    String reasonTemplate = getSpecificProperty("reason", true, true, false);

    // Analyze each ISBN
    boolean result = false;
    String contents = analysis.getContents();
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      String isbnNumber = isbn.getISBN();
      if (isbnNumber != null) {
        int length = isbnNumber.length();
        if ((length != 10) && (length != 13)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), isbn.getBeginIndex(), isbn.getEndIndex());
          errorResult.addPossibleAction(
              GT._(
                  "The ISBN''s length is {0} instead of 10 or 13",
                  Integer.toString(length) ),
              new NullActionProvider());
          String reason = null;
          if (reasonTemplate != null) {
            reason = GT._(reasonTemplate, Integer.toString(length));
          }
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
          if (helpNeededComment != null) {
            String replacement = isbn.askForHelp(helpNeededComment, reason);
            if (replacement != null) {
              errorResult.addReplacement(
                  contents.substring(isbn.getBeginIndex(), isbn.getEndIndex()) + replacement.toString(),
                  GT._("Add a comment"));
            }
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
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
        "reason", GT._("An explanation of the problem"));
    return parameters;
  }
}
