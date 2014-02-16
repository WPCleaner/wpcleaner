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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 521 of check wikipedia project.
 * Error 521: Date format in templates
 */
public class CheckErrorAlgorithm521 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm521() {
    super("Date format in templates");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Initialization
    String templatesProp = getSpecificProperty("templates", true, true, false);
    if (templatesProp == null) {
      return false;
    }
    List<String> checks = WPCConfiguration.convertPropertyToStringList(templatesProp);
    if ((checks == null) || (checks.isEmpty())) {
      return false;
    }
    String monthsProp = getSpecificProperty("months", true, true, false);
    List<String> months = (monthsProp != null) ?
        WPCConfiguration.convertPropertyToStringList(monthsProp) :
        null;

    // Search for incorrect formatting
    boolean result = false;
    for (String check : checks) {
      String[] elements = check.split("\\|");
      if ((elements != null) && (elements.length > 2)) {
        List<PageElementTemplate> templates = analysis.getTemplates(elements[0]);
        for (PageElementTemplate template : templates) {
          int paramIndex = template.getParameterIndex(elements[1]);
          if (paramIndex >= 0) {
            String value = template.getParameterValue(paramIndex);
            if ((value != null) && (value.trim().length() > 0)) {
              boolean formatOK = false;
              for (int i = 2; i < elements.length; i++) {
                formatOK |= checkFormat(value, elements[i], months);
              }
  
              // Report error
              if (!formatOK) {
                if (errors == null) {
                  return true;
                }
                result = true;
                int beginIndex = template.getParameterValueOffset(paramIndex);
                int endIndex = (paramIndex + 1 < template.getParameterCount()) ?
                    template.getParameterPipeOffset(paramIndex + 1) : template.getEndIndex() - 2;
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis.getPage(), beginIndex, endIndex);
                for (int i = 2; i < elements.length; i++) {
                  errorResult.addPossibleAction(elements[i], new NullActionProvider());
                }
                errors.add(errorResult);
              }
            }
          }
        }
      }
    }

    return result;
  }

  /**
   * Check format.
   * 
   * @param value Parameter value.
   * @param format Expected format.
   * @param months Possible values for months.
   * @return True if the value matches the format.
   */
  private boolean checkFormat(
      String value, String format,
      List<String> months) {
    if ((value == null) || (format == null)) {
      return false;
    }
    int valueIndex = 0;
    int formatIndex = 0;
    while ((valueIndex < value.length()) && (formatIndex < format.length())) {
      char formatChar = format.charAt(formatIndex);
      char valueChar = value.charAt(valueIndex);
      if (formatChar == '\'') {
        // Check for quoted text or quote
        // TODO
      } else if ((formatChar == 'd') ||
                 (formatChar == 'M') ||
                 (formatChar == 'y')) {
        // Count the number of consecutive identical formatting characters
        int formatCount = 0;
        while ((formatIndex < format.length()) &&
               (format.charAt(formatIndex) == formatChar)) {
          formatCount++;
          formatIndex++;
        }

        // Count the number of consecutive digits
        int digitCount = 0;
        while ((valueIndex < value.length()) &&
               (Character.isDigit(value.charAt(valueIndex)))) {
          digitCount++;
          valueIndex++;
        }

        // Check format
        if (formatChar == 'd') {

          // Check format of day field
          if (formatCount == 1) {
            // Format "d": day number without leading 0
            if ((digitCount == 0) || (digitCount > 2) || (valueChar == '0')) {
              return false;
            }
          } else {
            // Format "dd": day number with leading 0
            if (digitCount != 2) {
              return false;
            }
          }

        } else if (formatChar == 'M') {

          // Check format of month field
          if (formatCount == 1) {
            // Format "M": month number without leading 0
            if ((digitCount == 0) || (digitCount > 2) || (valueChar == '0')) {
              return false;
            }
          } else if (formatCount == 2) {
            // Format "MM": month number with leading 0
            if (digitCount != 2) {
              return false;
            }
          } else {
            // Format "MMM": month name
            if (digitCount != 0) {
              return false;
            }
            if (months == null) {
              return false;
            }
            boolean monthFound = false;
            for (String month : months) {
              if (!monthFound && value.startsWith(month, valueIndex)) {
                valueIndex += month.length();
                monthFound = true;
              }
            }
            if (!monthFound) {
              return false;
            }
          }

        } else if (formatChar == 'y') {

          // Check format of year field
          if ((formatCount == 1) || (formatCount == 2)) {
            // Format "Y" or "YY": year on 2 digits
            if (digitCount != 2) {
              return false;
            }
          } else {
            // Format "YYY": year on as many digits as required
            if ((digitCount == 0) || (valueChar == '0')) {
              return false;
            }
          }
        }

      } else {

        // Check for identical text
        if (value.charAt(valueIndex) != format.charAt(formatIndex)) {
          return false;
        }
        valueIndex++;
        formatIndex++;
      }
    }
    if ((valueIndex < value.length()) || (formatIndex < format.length())) {
      return false;
    }
    return true;
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("templates", GT._("A list of templates and parameters in which format should be checked"));
    return parameters;
  }
}
