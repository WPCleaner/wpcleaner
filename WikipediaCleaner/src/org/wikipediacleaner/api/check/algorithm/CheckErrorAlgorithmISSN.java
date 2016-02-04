/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.SearchEngine;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.action.ActionMultiple;
import org.wikipediacleaner.i18n.GT;


/**
 * Base class for errors on ISSN numbers.
 */
public abstract class CheckErrorAlgorithmISSN extends CheckErrorAlgorithmBase {

  /**
   * @param name Algorithm name.
   */
  protected CheckErrorAlgorithmISSN(String name) {
    super(name);
  }

  /**
   * @param analysis Page analysis.
   * @param issn ISSN.
   * @param checkForComment True to check for a comment after the ISSN.
   * @return Error result.
   */
  protected CheckErrorResult createCheckErrorResult(
      PageAnalysis analysis, PageElementISSN issn,
      boolean checkForComment) {
    ErrorLevel level = (issn.isValid() && !issn.helpRequested()) ?
        ErrorLevel.ERROR : ErrorLevel.WARNING;
    if (checkForComment) {
      String contents = analysis.getContents();
      int index = issn.getEndIndex();
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
        analysis, issn.getBeginIndex(), issn.getEndIndex(), level);
    return result;
  }

  private final String[] possibleSplit = {
      "[,/]",
      "[,/ ]"
  };

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param issn ISSN.
   */
  protected void addSuggestions(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISSN issn) {
    if ((analysis == null) || (issn == null)) {
      return;
    }

    // Split ISSN in several potential ISSN
    List<String> issnValues = new ArrayList<String>();
    if (issn.isTemplateParameter()) {

      // Basic splits
      for (String split : possibleSplit) {
        issnValues.clear();
        for (String value : issn.getISSNNotTrimmed().trim().split(split)) {
          issnValues.add(value);
        }
        addSuggestions(analysis, errorResult, issn, issnValues);
      }

      // Evolved split
      String issnValue = issn.getISSNNotTrimmed().trim();
      issnValues.clear();
      while (issnValue.length() > 0) {
        // Remove extra characters
        int index = 0;
        while ((index < issnValue.length()) &&
               (!Character.isDigit(issnValue.charAt(index)))) {
          index++;
        }

        // Find characters
        if (index > 0) {
          issnValue=  issnValue.substring(index);
        }
        index = 0;
        while ((index < issnValue.length()) &&
               (!Character.isLetter(issnValue.charAt(index))) &
               (Character.toUpperCase(issnValue.charAt(index)) != 'X')) {
          index++;
        }
        if (index > 0) {
          issnValues.add(issnValue.substring(0, index));
          issnValue = issnValue.substring(index);
        }
      }
      addSuggestions(analysis, errorResult, issn, issnValues);
    } else {
      issnValues.add(issn.getISSNNotTrimmed());
      addSuggestions(analysis, errorResult, issn, issnValues);
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param issn ISSN.
   * @param issnValues Broken down ISSN values.
   */
  private void addSuggestions(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISSN issn, List<String> issnValues) {
    // Remove empty ISSN
    Iterator<String> itValues = issnValues.iterator();
    while (itValues.hasNext()) {
      String value = itValues.next();
      if ((value == null) || (value.trim().length() == 0)) {
        itValues.remove();
      }
    }

    // Cleanup potential ISSN
    final String extraChars = " ():./";
    for (int numIssn = 0; numIssn < issnValues.size(); numIssn++) {
      String issnValue = issnValues.get(numIssn);

      // Remove extra characters at the beginning
      while ((issnValue.length() > 0) &&
             (extraChars.indexOf(issnValue.charAt(0)) >= 0)) {
        issnValue = issnValue.substring(1);
      }

      // Remove ISSN prefix
      if (issn.isTemplateParameter()) {
        if (issnValue.toUpperCase().startsWith("ISSN")) {
          issnValue = issnValue.substring(4);
        }
      }

      // Remove extra characters at the beginning
      while ((issnValue.length() > 0) &&
             (extraChars.indexOf(issnValue.charAt(0)) >= 0)) {
        issnValue = issnValue.substring(1);
      }

      // Remove extra characters at both extremities
      while ((issnValue.length() > 0) &&
             (extraChars.indexOf(issnValue.charAt(0)) >= 0)) {
        issnValue = issnValue.substring(1);
      }
      while ((issnValue.length() > 0) &&
             (extraChars.indexOf(issnValue.charAt(issnValue.length() - 1)) >= 0)) {
        issnValue = issnValue.substring(0, issnValue.length() - 1);
      }
      issnValues.set(numIssn, issnValue);
    }

    // Remove empty ISSN
    itValues = issnValues.iterator();
    while (itValues.hasNext()) {
      String value = itValues.next();
      if ((value == null) || (value.trim().length() == 0)) {
        itValues.remove();
      }
    }

    // Check if all potential ISSN are valid
    boolean correctLength = true;
    for (String issnValue : issnValues) {
      String cleanISSN = PageElementISSN.cleanISSN(issnValue);
      int length = cleanISSN.length();
      if (length != 8) {
        correctLength = false;
      }
    }

    // Suggestions
    if (correctLength) {
      if (issnValues.size() == 1) {
        String value = issnValues.get(0);
        if (!value.equals(issn.getISSNNotTrimmed())) {
          errorResult.addReplacement(issnValues.get(0));
        }
      } else if (issnValues.size() > 0) {
        if (issn.isTemplateParameter()) {
          PageElementTemplate template = analysis.isInTemplate(issn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(issn.getBeginIndex());
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
              buffer.append(issnValues.get(0));
              for (int issnNum = 1; issnNum < issnValues.size(); issnNum++) {
                while (template.getParameterIndex(name + Integer.toString(currentNum)) >= 0) {
                  currentNum++;
                }
                buffer.append(" |");
                buffer.append(name);
                buffer.append(Integer.toString(currentNum));
                buffer.append("=");
                buffer.append(issnValues.get(issnNum));
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
   * @param issn ISSN.
   */
  protected void addHelpNeededTemplates(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISSN issn) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> helpNeededTemplates = config.getStringArrayList(
        WPCConfigurationStringList.ISSN_HELP_NEEDED_TEMPLATES);
    if ((helpNeededTemplates != null) &&
        (!helpNeededTemplates.isEmpty())) {
      String reason = getReason(issn);
      for (String[] helpNeededTemplate : helpNeededTemplates) {
        String replacement = issn.askForHelp(helpNeededTemplate, reason);
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
   * @param issn ISSN.
   */
  protected void addHelpNeededComment(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementISSN issn) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    String helpNeededComment = config.getString(
        WPCConfigurationString.ISSN_HELP_NEEDED_COMMENT);
    if (helpNeededComment != null) {
      String reason = getReason(issn);
      String replacement = issn.askForHelp(helpNeededComment, reason);
      if (replacement != null) {
        String contents = analysis.getContents();
        replacement =
            contents.substring(issn.getBeginIndex(), issn.getEndIndex()) +
            replacement;
        errorResult.addReplacement(replacement, GT._("Add a comment"));
      }
    }
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error result.
   * @param searches List of strings to search.
   * @param title Title for the group of searches.
   */
  protected void addSearchEngines(
      PageAnalysis analysis, CheckErrorResult errorResult,
      List<String> searches, String title) {

    // Check configuration
    if ((searches == null) || searches.isEmpty()) {
      return;
    }
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> searchEngines = config.getStringArrayList(
        WPCConfigurationStringList.ISSN_SEARCH_ENGINES);
    if ((searchEngines == null) || searchEngines.isEmpty()) {
      return;
    }

    // Add title
    if ((title != null) && (searches.size() > 1)) {
      errorResult.addPossibleAction(new SimpleAction(title, null));
    }

    // Create global actions
    if (searches.size() > 1) {
      List<Actionnable> actions = new ArrayList<>();
      for (String[] searchEngine : searchEngines) {
        if (searchEngine.length > 1) {
          ActionMultiple action = new ActionMultiple();
          for (String search : searches) {
            try {
              action.addAction(
                  new ActionExternalViewer(MessageFormat.format(searchEngine[1], search)));
            } catch (IllegalArgumentException e) {
              //
            }
          }
          actions.add(new SimpleAction(searchEngine[0], action));
        }
      }
      errorResult.addPossibleAction(new CompositeAction(
          GT._("Search all ISSN"), actions));
    }

    // Create unit actions
    for (String search : searches) {
      List<Actionnable> actions = new ArrayList<>();
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
   * @param search String to search.
   */
  protected void addSearchEngines(
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
   * @param search String to search.
   */
  protected void addSearchEnginesISBN(
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
   * @param template Template in which the ISSN is.
   */
  protected void addSearchEngines(
      PageAnalysis analysis, CheckErrorResult errorResult,
      PageElementTemplate template) {
    if (template == null) {
      return;
    }

    // Add search engines
    Map<String, List<SearchEngine>> searchEngines = SearchEngine.getSearchEngines(
        analysis.getWikipedia(), template,
        WPCConfigurationStringList.ISSN_SEARCH_ENGINES_TEMPLATES);
    if (searchEngines == null) {
      return;
    }
    List<String> parameterNames = new ArrayList<>(searchEngines.keySet());
    Collections.sort(parameterNames);
    for (String parameterName : parameterNames) {
      List<Actionnable> actions = new ArrayList<>();
      for (SearchEngine searchEngine : searchEngines.get(parameterName)) {
        actions.add(new SimpleAction(
            searchEngine.getName(),
            new ActionExternalViewer(searchEngine.getUrl())));
      }
      errorResult.addPossibleAction(new CompositeAction(
          GT._("Search using {0}", parameterName), actions));
    }
  }

  /**
   * @param issn ISSN number.
   * @return Reason for the error.
   */
  public abstract String getReason(PageElementISSN issn);
}
