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
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 69 of check wikipedia project.
 * Error 69: ISBN wrong syntax
 */
public class CheckErrorAlgorithm069 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm069() {
    super("ISBN wrong syntax");
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

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      boolean isError = false;
      if (!isbn.isCorrect() && isbn.isValid()) {
        isError = true;
      }

      // Exclude special configured values for ISBN
      if (isError && isbn.isTemplateParameter()) {
        WPCConfiguration config = analysis.getWPCConfiguration();
        List<String[]> specialValues = config.getStringArrayList(
            WPCConfigurationStringList.ISBN_SPECIAL_VALUES);
        if ((specialValues != null) && !specialValues.isEmpty()) {
          PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(isbn.getBeginIndex());
            if ((param != null) &&
                (param.getName() != null) &&
                (param.getName().trim().length() > 0)) {
              String name = param.getName().trim();
              for (String[] specialValue : specialValues) {
                if ((specialValue.length > 2) &&
                    (Page.areSameTitle(template.getTemplateName(), specialValue[0])) &&
                    (name.equals(specialValue[1])) &&
                    (isbn.getISBNNotTrimmed().equals(specialValue[2]))) {
                  isError = false;
                }
              }
            }
          }
        }
      }

      // Exclude parameters in templates
      if (isError &&
          isbn.isTemplateParameter() &&
          analysis.isInNamespace(Namespace.TEMPLATE)) {
        PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
        if (template != null) {
          Parameter param = template.getParameterAtIndex(isbn.getBeginIndex());
          if (param != null) {
            List<PageElementFunction> functions = analysis.getFunctions();
            if (functions != null) {
              for (PageElementFunction function : functions) {
                int functionIndex = function.getBeginIndex();
                if ((template == analysis.isInTemplate(functionIndex)) &&
                    (param == template.getParameterAtIndex(functionIndex))) {
                  isError = false;
                }
              }
            }
          }
        }
      }

      // Report error
      if (isError) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, false);
        addSuggestions(analysis, errorResult, isbn);
        errors.add(errorResult);
        List<String> replacements = isbn.getCorrectISBN();
        if (replacements != null) {
          for (String replacement : replacements) {
            if (!replacement.equals(analysis.getContents().substring(isbn.getBeginIndex(), isbn.getEndIndex()))) {
              errorResult.addReplacement(replacement);
            }
          }
        }
      }
    }

    // Analyze each template parameter for ISBN10 or ISBN13 parameters
    /* List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        Parameter param = template.getParameter(paramNum);
        String paramName = param.getName();
        if ("ISBN10".equalsIgnoreCase(paramName) ||
            "ISBN13".equalsIgnoreCase(paramName)) {
          if (errors == null) {
            return true;
          }
          result = true;

          // For an empty parameter, suggest to remove the parameter
          String value = param.getStrippedValue();
          if ((value == null) || (value.trim().length() == 0)) {
            int begin = param.getPipeIndex();
            int end = template.getEndIndex() - 2;
            if (paramNum + 1 < template.getParameterCount()) {
              end = template.getParameter(paramNum + 1).getPipeIndex();
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, begin, end,
                ErrorLevel.WARNING);
            errorResult.addReplacement("");
            errors.add(errorResult);

          // For a parameter with a value, suggest the first available name
          } else {
            int begin = template.getParameterNameStartIndex(paramNum);
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, begin, begin + paramName.length(),
                ErrorLevel.WARNING);
            boolean found = false;
            int number = 1;
            while ((number < 10) && !found) {
              boolean exist = false;
              for (int paramNum2 = 0; paramNum2 < template.getParameterCount(); paramNum2++) {
                String paramName2 = template.getParameterName(paramNum2);
                if ((number == 1) && ("ISBN".equalsIgnoreCase(paramName2))) {
                  exist = true;
                }
                if (("ISBN" + number).equalsIgnoreCase(paramName2)) {
                  exist = true;
                }
              }
              if (!exist) {
                found = true;
                if (number == 1) {
                  errorResult.addReplacement(paramName.substring(0, 4));
                } else {
                  errorResult.addReplacement(paramName.substring(0, 4) + number);
                }
              }
              number++;
            }
            errors.add(errorResult);
          }
        }
      }
    }*/

    return result;
  }

  /**
   * @param isbn ISBN number.
   * @return Reason for the error.
   */
  @Override
  public String getReason(PageElementISBN isbn) {
    if (isbn == null) {
      return null;
    }
    String reasonTemplate = getSpecificProperty("reason", true, true, false);
    if (reasonTemplate == null) {
      return null;
    }
    return reasonTemplate;
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
