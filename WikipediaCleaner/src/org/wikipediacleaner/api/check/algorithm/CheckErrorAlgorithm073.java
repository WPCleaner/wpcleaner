/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 73 of check wikipedia project.
 * Error 73: ISBN wrong checksum in ISBN-13
 */
public class CheckErrorAlgorithm073 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm073() {
    super("ISBN wrong checksum in ISBN-13");
  }

  /**
   * @return Flag indicating if the detection is fully done.
   */
  @Override
  public boolean isFullDetection() {
    return false;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
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
      String number = isbn.getISBN();
      if ((number != null) && (number.length() == 13)) {
        char check = Character.toUpperCase(number.charAt(12));
        char computedCheck = Character.toUpperCase(isbn.getCheck());
        if ((check != computedCheck) && Character.isDigit(computedCheck)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), isbn.getBeginIndex(), isbn.getEndIndex());
          errorResult.addPossibleAction(
              GT._(
                  "The checksum is {0} instead of {1}",
                  new Object[] { check, computedCheck } ),
              new NullActionProvider());
          String reason = null;
          if (reasonTemplate != null) {
            reason = GT._(reasonTemplate, new Object[] { computedCheck, check });
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
}
