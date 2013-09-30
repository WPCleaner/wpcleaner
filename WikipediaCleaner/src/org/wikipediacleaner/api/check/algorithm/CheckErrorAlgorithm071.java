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
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 71 of check wikipedia project.
 * Error 71: ISBN wrong position of X
 */
public class CheckErrorAlgorithm071 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm071() {
    super("ISBN wrong position of X");
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
        boolean found = false;
        for (int i = 0; i < isbnNumber.length(); i++) {
          if (Character.toUpperCase(isbnNumber.charAt(i)) == 'X') {
            if ((i != 9) || (isbnNumber.length() != 10)) {
              found = true;
            }
          }
        }

        if (found) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), isbn.getBeginIndex(), isbn.getEndIndex());
          String reason = null;
          if (reasonTemplate != null) {
            reason = GT._(reasonTemplate);
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
