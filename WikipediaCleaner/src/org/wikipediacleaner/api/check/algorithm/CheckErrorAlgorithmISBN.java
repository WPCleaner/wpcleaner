/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.ISBNRange;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.ISBNRange.ISBNInformation;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
    ErrorLevel level = (isbn.isValid() && !isbn.helpRequested()) ?
        ErrorLevel.ERROR : ErrorLevel.WARNING;
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
        analysis, isbn.getBeginIndex(), isbn.getEndIndex(), level);
    ISBNInformation infos = ISBNRange.getInformation(isbn.getISBN());
    if ((infos != null) && (infos.getTexts() != null)) {
      for (String info : infos.getTexts()) {
        result.addPossibleAction(info, new NullActionProvider());
      }
    }
    return result;
  }

  private final String[] possibleSplit = {
      "[,/]",
      "[,/ ]"
  };

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param isbn ISBN.
   */
  protected void addSuggestions(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISBN isbn) {
    if ((analysis == null) || (isbn == null)) {
      return;
    }

    // Split ISBN in several potential ISBN
    List<String> isbnValues = new ArrayList<String>();
    if (isbn.isTemplateParameter()) {

      // Basic splits
      for (String split : possibleSplit) {
        isbnValues.clear();
        for (String value : isbn.getISBNNotTrimmed().trim().split(split)) {
          isbnValues.add(value);
        }
        addSuggestions(analysis, errorResult, isbn, isbnValues);
      }

      // Evolved split
      String isbnValue = isbn.getISBNNotTrimmed().trim();
      isbnValues.clear();
      while (isbnValue.length() > 0) {
        // Remove extra characters
        int index = 0;
        while ((index < isbnValue.length()) &&
               (!Character.isDigit(isbnValue.charAt(index)))) {
          index++;
        }

        // Find characters
        if (index > 0) {
          isbnValue=  isbnValue.substring(index);
        }
        index = 0;
        while ((index < isbnValue.length()) &&
               (!Character.isLetter(isbnValue.charAt(index))) &
               (Character.toUpperCase(isbnValue.charAt(index)) != 'X')) {
          index++;
        }
        if (index > 0) {
          isbnValues.add(isbnValue.substring(0, index));
          isbnValue = isbnValue.substring(index);
        }
      }
      addSuggestions(analysis, errorResult, isbn, isbnValues);
    } else {
      isbnValues.add(isbn.getISBNNotTrimmed());
      addSuggestions(analysis, errorResult, isbn, isbnValues);
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param isbn ISBN.
   * @param isbnValues Broken down ISBN values.
   */
  private void addSuggestions(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISBN isbn, List<String> isbnValues) {
    // Remove empty ISBN
    Iterator<String> itValues = isbnValues.iterator();
    while (itValues.hasNext()) {
      String value = itValues.next();
      if ((value == null) || (value.trim().length() == 0)) {
        itValues.remove();
      }
    }

    // Cleanup potential ISBN
    final String extraChars = " ():./";
    for (int numIsbn = 0; numIsbn < isbnValues.size(); numIsbn++) {
      String isbnValue = isbnValues.get(numIsbn);

      // Remove extra characters at the beginning
      while ((isbnValue.length() > 0) &&
             (extraChars.indexOf(isbnValue.charAt(0)) >= 0)) {
        isbnValue = isbnValue.substring(1);
      }

      // Remove ISBN prefix
      if (isbn.isTemplateParameter()) {
        if (isbnValue.toUpperCase().startsWith("ISBN")) {
          isbnValue = isbnValue.substring(4);
        }
      }

      // Remove extra characters at the beginning
      while ((isbnValue.length() > 0) &&
             (extraChars.indexOf(isbnValue.charAt(0)) >= 0)) {
        isbnValue = isbnValue.substring(1);
      }

      // Remove ISBN-10 or ISBN-13 prefix
      String cleanISBN = PageElementISBN.cleanISBN(isbnValue);
      if (((cleanISBN.length() == 12) && (cleanISBN.startsWith("10"))) ||
          ((cleanISBN.length() == 15) && (cleanISBN.startsWith("13")))) {
        int digitCount = 0;
        int index = 0;
        boolean ok = true;
        while ((index < isbnValue.length()) && (digitCount < 3)) {
          char current = isbnValue.charAt(index);
          if (Character.isDigit(current)) {
            digitCount++;
          } else if (Character.isLetter(current)) {
            ok = false;
          }
          if (digitCount < 3) {
            index++;
          }
        }
        if (ok) {
          isbnValue = isbnValue.substring(index);
        }
      }

      // Remove extra characters at both extremities
      while ((isbnValue.length() > 0) &&
             (extraChars.indexOf(isbnValue.charAt(0)) >= 0)) {
        isbnValue = isbnValue.substring(1);
      }
      while ((isbnValue.length() > 0) &&
             (extraChars.indexOf(isbnValue.charAt(isbnValue.length() - 1)) >= 0)) {
        isbnValue = isbnValue.substring(0, isbnValue.length() - 1);
      }
      isbnValues.set(numIsbn, isbnValue);
    }

    // Remove empty ISBN
    itValues = isbnValues.iterator();
    while (itValues.hasNext()) {
      String value = itValues.next();
      if ((value == null) || (value.trim().length() == 0)) {
        itValues.remove();
      }
    }

    // Check if all potential ISBN are valid
    boolean correctLength = true;
    for (String isbnValue : isbnValues) {
      String cleanISBN = PageElementISBN.cleanISBN(isbnValue);
      int length = cleanISBN.length();
      if ((length != 10) && (length != 13)) {
        correctLength = false;
      }
    }

    // Suggestions
    if (correctLength) {
      if (isbnValues.size() == 1) {
        String value = isbnValues.get(0);
        if (!value.equals(isbn.getISBNNotTrimmed())) {
          errorResult.addReplacement(isbnValues.get(0));
        }
      } else if (isbnValues.size() > 0) {
        if (isbn.isTemplateParameter()) {
          PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(isbn.getBeginIndex());
            if ((param != null) &&
                (param.getName() != null) &&
                (param.getName().trim().length() > 0)) {
              String name = param.getName().trim();
              int index = name.length();
              while ((index > 0) &&
                     (Character.isDigit(name.charAt(index - 1)))) {
                index--;
              }
              int currentNum = 1;
              if (index < name.length()) {
                currentNum = Integer.valueOf(name.substring(index));
                name = name.substring(0, index);
              }
              currentNum++;
              StringBuilder buffer = new StringBuilder();
              buffer.append(isbnValues.get(0));
              for (int isbnNum = 1; isbnNum < isbnValues.size(); isbnNum++) {
                while (template.getParameterIndex(name + Integer.toString(currentNum)) >= 0) {
                  currentNum++;
                }
                buffer.append(" |");
                buffer.append(name);
                buffer.append(Integer.toString(currentNum));
                buffer.append("=");
                buffer.append(isbnValues.get(isbnNum));
                currentNum++;
              }
              errorResult.addReplacement(buffer.toString());
            }
          }
        }
      }
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param isbn ISBN.
   */
  protected void addHelpNeededTemplates(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISBN isbn) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> helpNeededTemplates = config.getStringArrayList(
        WPCConfigurationStringList.ISBN_HELP_NEEDED_TEMPLATES);
    if ((helpNeededTemplates != null) &&
        (!helpNeededTemplates.isEmpty())) {
      String reason = getReason(isbn);
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
   */
  protected void addHelpNeededComment(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISBN isbn) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    String helpNeededComment = config.getString(
        WPCConfigurationString.ISBN_HELP_NEEDED_COMMENT);
    if (helpNeededComment != null) {
      String reason = getReason(isbn);
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

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param search String to search.
   */
  protected void addSearchEngines(
      PageAnalysis analysis, CheckErrorResult errorResult,
      String search) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> searchEngines = config.getStringArrayList(
        WPCConfigurationStringList.ISBN_SEARCH_ENGINES);
    if ((searchEngines != null) &&
        (!searchEngines.isEmpty())) {
      List<Actionnable> actions = new ArrayList<Actionnable>();
      for (String[] searchEngine : searchEngines) {
        try {
          if (searchEngine.length > 1) {
            actions.add(new SimpleAction(
                searchEngine[0],
                new ActionExternalViewer(MessageFormat.format(searchEngine[1], search))));
          }
        } catch (IllegalArgumentException e) {
          //
        }
      }
      errorResult.addPossibleAction(new CompositeAction(
          GT._("Search ISBN {0}", search), actions));
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param search String to search.
   */
  protected void addSearchEnginesISSN(
      PageAnalysis analysis, CheckErrorResult errorResult,
      String search) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> searchEngines = config.getStringArrayList(
        WPCConfigurationStringList.ISSN_SEARCH_ENGINES);
    if ((searchEngines != null) &&
        (!searchEngines.isEmpty())) {
      List<Actionnable> actions = new ArrayList<Actionnable>();
      for (String[] searchEngine : searchEngines) {
        try {
          if (searchEngine.length > 1) {
            actions.add(new SimpleAction(
                searchEngine[0],
                new ActionExternalViewer(MessageFormat.format(searchEngine[1], search))));
          }
        } catch (IllegalArgumentException e) {
          //
        }
      }
      errorResult.addPossibleAction(new CompositeAction(
          GT._("Search ISSN {0}", search), actions));
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param template Template in which the ISBN is.
   */
  protected void addSearchEngines(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementTemplate template) {
    if (template == null) {
      return;
    }
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> searchEngines = config.getStringArrayList(
        WPCConfigurationStringList.ISBN_SEARCH_ENGINES_TEMPLATES);
    if (searchEngines == null) {
      return;
    }

    // Keep only search engines relative to the template
    int index = 0;
    while (index < searchEngines.size()) {
      boolean keep = false;
      String[] searchEngine = searchEngines.get(index);
      if ((searchEngine.length >= 4) &&
          (Page.areSameTitle(template.getTemplateName(), searchEngine[2]))) {
        String value = template.getParameterValue(searchEngine[3]);
        if ((value != null) && (value.trim().length() > 0)) {
          keep = true;
        }
      }
      if (keep) {
        index++;
      } else {
        searchEngines.remove(index);
      }
    }

    // Add search engines
    while (!searchEngines.isEmpty()) {
      String paramName = searchEngines.get(0)[3].trim();
      String paramValue = template.getParameterValue(paramName);
      List<Actionnable> actions = new ArrayList<Actionnable>();
      index = 0;
      while (index < searchEngines.size()) {
        String[] searchEngine = searchEngines.get(index);
        if (paramName.equals(searchEngine[3].trim())) {
          try {
            actions.add(new SimpleAction(
                searchEngine[0],
                new ActionExternalViewer(MessageFormat.format(
                    searchEngine[1], URLEncoder.encode(paramValue, "UTF8")))));
          } catch (UnsupportedEncodingException e) {
            // Nothing to do
          }
          searchEngines.remove(index);
        } else {
          index++;
        }
      }
      errorResult.addPossibleAction(new CompositeAction(
          GT._("Search using {0}", paramName), actions));
    }
  }

  /**
   * @param isbn ISBN number.
   * @return Reason for the error.
   */
  public abstract String getReason(PageElementISBN isbn);
}
