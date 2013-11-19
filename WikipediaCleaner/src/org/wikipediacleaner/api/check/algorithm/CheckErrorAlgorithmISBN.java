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
import java.util.List;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.CompositeAction;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
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
}
