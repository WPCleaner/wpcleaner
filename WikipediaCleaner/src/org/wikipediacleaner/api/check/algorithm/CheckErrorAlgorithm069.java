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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
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

        // Check for potential extra characters around
        int beginIndex = isbn.getBeginIndex();
        int endIndex = isbn.getEndIndex();
        String contents = analysis.getContents();
        boolean tryAgain = true;
        while (tryAgain) {
          tryAgain = false;
          if ((beginIndex > 0) && (endIndex < contents.length())) {
            char previousChar = contents.charAt(beginIndex - 1);
            char nextChar = contents.charAt(endIndex);
            if (((previousChar == '(') && (nextChar == ')')) ||
                ((previousChar == '[') && (nextChar == ']'))) {
              beginIndex--;
              endIndex++;
              tryAgain = true;
            }
          }
        }
        final String SMALL_OPEN = "<small>";
        final String SMALL_CLOSE = "</small>";
        if ((beginIndex >= SMALL_OPEN.length()) && (endIndex < contents.length())) {
          if (contents.startsWith(SMALL_OPEN, beginIndex - SMALL_OPEN.length()) &&
              contents.startsWith(SMALL_CLOSE, endIndex)) {
            beginIndex -= SMALL_OPEN.length();
            endIndex += SMALL_CLOSE.length();
          }
        }

        CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, false);
        String prefix = null;
        String suffix = null;
        if ((beginIndex < isbn.getBeginIndex()) && (endIndex > isbn.getEndIndex())) {
          prefix = contents.substring(beginIndex, isbn.getBeginIndex());
          suffix = contents.substring(isbn.getEndIndex(), endIndex);
          errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex, errorResult.getErrorLevel());
        }
        addSuggestions(analysis, errorResult, isbn);
        errors.add(errorResult);
        List<String> replacements = isbn.getCorrectISBN();
        if (replacements != null) {
          for (String replacement : replacements) {
            if (!replacement.equals(analysis.getContents().substring(isbn.getBeginIndex(), isbn.getEndIndex()))) {
              if ((prefix != null) && (suffix != null)) {
                errorResult.addReplacement(prefix + replacement + suffix);
              }
              errorResult.addReplacement(replacement);
            }
          }
        }
      } else {
        if (!isbn.isTemplateParameter()) {
          // Analyze to find links to Special/BookSources
          PageElement element = null;
          ErrorLevel level = ErrorLevel.CORRECT;
          String isbnText = analysis.getContents().substring(isbn.getBeginIndex(), isbn.getEndIndex());
          PageElementInternalLink link = analysis.isInInternalLink(isbn.getBeginIndex());
          if ((link != null) && (isbnText.equals(link.getText()))) {
            level = isSpecialBookSources(analysis, link.getLink());
            if (level != ErrorLevel.CORRECT) {
              element = link;
            }
          }
          if (element == null) {
            PageElementInterwikiLink iwLink = analysis.isInInterwikiLink(isbn.getBeginIndex());
            if ((iwLink != null) && (isbnText.equals(iwLink.getText()))) {
              level = isSpecialBookSources(analysis, iwLink.getLink());
              if (level != ErrorLevel.CORRECT) {
                element = iwLink;
              }
            }
          }
          if (element != null) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, element.getBeginIndex(), element.getEndIndex(), level);
            List<String> replacements = isbn.getCorrectISBN();
            for (String replacement : replacements) {
              errorResult.addReplacement(replacement);
            }
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * @param analysis Page analysis.
   * @param link Link destination.
   * @return Error level.
   */
  private ErrorLevel isSpecialBookSources(PageAnalysis analysis, String link) {
    if (link == null) {
      return ErrorLevel.CORRECT;
    }
    int colonIndex = link.indexOf(':');
    if (colonIndex == 0) {
      link = link.substring(1);
      colonIndex = link.indexOf(':');
    }
    if (colonIndex > 0) {
      Namespace special = analysis.getWikiConfiguration().getNamespace(Namespace.SPECIAL);
      String prefix = link.substring(0, colonIndex);
      if ((special != null) && (special.isPossibleName(prefix))) {
        if (link.startsWith("BookSources", colonIndex + 1)) {
          return ErrorLevel.ERROR;
        }
        return ErrorLevel.WARNING;
      }
    }
    return ErrorLevel.CORRECT;
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
