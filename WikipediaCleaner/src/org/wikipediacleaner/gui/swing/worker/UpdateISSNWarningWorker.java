/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.api.data.contents.ContentsComment;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateWarningTools.Stats;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * SwingWorker for updating ISSN warning.
 */
public class UpdateISSNWarningWorker extends UpdateWarningWorker {

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param simulation True if this is a simulation.
   */
  public UpdateISSNWarningWorker(
      EnumWikipedia wiki, BasicWindow window,
      boolean simulation) {
    super(wiki, window, null, simulation);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateISSNWarningWorker(
      EnumWikipedia wiki, BasicWindow window, List<Page> pages,
      boolean contentsAvailable, boolean automaticEdit) {
    super(wiki, window, pages, contentsAvailable, automaticEdit);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    long startTime = System.currentTimeMillis();
    EnumWikipedia wiki = getWikipedia();
    int lastCount = 0;
    Stats stats = new Stats();
    Map<String, List<String>> errors = null;
    UpdateISSNWarningTools tools = new UpdateISSNWarningTools(wiki, this, true, automaticEdit);
    try {
      if (!useList) {
        listWarningPages(tools);

        // Ask for confirmation
        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._(
              "Analysis found {0} articles to check for {1} errors.\n" +
              "Do you want to update the warnings ?",
              new Object[] { Integer.valueOf(warningPages.size()).toString(), "ISSN" } ));
          if (answer != JOptionPane.YES_OPTION) {
            return Integer.valueOf(0);
          }
        }

        // Sort the list of articles
        Collections.sort(warningPages, PageComparator.getTitleFirstComparator());
        if (warningPages.isEmpty()) {
          return Integer.valueOf(0);
        }
      }

      // Working with sublists
      tools.setContentsAvailable(contentsAvailable);
      tools.prepareErrorsMap();
      if (simulation) {
        tools.setSimulation(true);
      }
      String lastTitle = null;
      while (!warningPages.isEmpty()) {
        // Creating sublist
        List<Page> sublist = tools.extractSublist(warningPages, 10, false);
        if (sublist.isEmpty()) {
          errors = tools.getErrorsMap();
          displayResult(stats, startTime, errors);
          return Integer.valueOf(stats.getUpdatedPagesCount());
        }

        // Update warning
        boolean finish = false;
        while (!finish) {
          finish = true;
          try {
            lastTitle = sublist.get(sublist.size() - 1).getTitle();
            tools.updateWarning(sublist, null, null, stats);
          } catch (APIException e) {
            if (getWindow() != null) {
              int answer = getWindow().displayYesNoWarning(GT._(
                  "An error occurred when updating {1} warnings. Do you want to continue ?\n\n" +
                  "Error: {0}",
                  new Object[] { e.getMessage(), "ISSN" }));
              if (answer != JOptionPane.YES_OPTION) {
                return e;
              }
              finish = false;
            }
          }
          if (shouldStop()) {
            Configuration config = Configuration.getConfiguration();
            config.setString(null, ConfigurationValueString.LAST_ISSN_WARNING, lastTitle);
            displayResult(stats, startTime, null);
            return Integer.valueOf(stats.getUpdatedPagesCount());
          }
        }

        if (stats.getUpdatedPagesCount() > lastCount) {
          lastCount = stats.getUpdatedPagesCount();
          /*if (getWindow() != null) {
            int answer = getWindow().displayYesNoWarning(
                "This feature is currently under development, please check the modification.\n" +
                "Do you want to continue ?");
            if (answer != JOptionPane.YES_OPTION) {
              return Integer.valueOf(lastCount);
            }
          } else {
            return Integer.valueOf(lastCount);
          }*/
        }
      }
      errors = tools.getErrorsMap();
      if (warningPages.isEmpty()) {
        Configuration config = Configuration.getConfiguration();
        config.setString(null, ConfigurationValueString.LAST_ISSN_WARNING, (String) null);
      }
    } catch (APIException e) {
      return e;
    }

    displayResult(stats, startTime, errors);
    return Integer.valueOf(stats.getUpdatedPagesCount());
  }

  /**
   * Generate the list of warning pages.
   * 
   * @param tools Update warning tools.
   * @throws APIException
   */
  @Override
  protected void listWarningPages(UpdateWarningTools tools) throws APIException {
    Map<String, Page> tmpWarningPages = new HashMap<String, Page>();

    // Retrieve talk pages including a warning
    retrieveArticlesWithWarning(
        WPCConfigurationString.ISSN_WARNING_TEMPLATE,
        tmpWarningPages);

    // Retrieve articles in categories for ISSN errors
    retrieveCategoryMembers(
        WPCConfigurationStringList.ISSN_ERRORS_CATEGORIES,
        tmpWarningPages);

    // Retrieve articles in lists for ISSN errors
    retrieveInternalLinks(
        WPCConfigurationStringList.ISSN_ERRORS_LISTS,
        tmpWarningPages);

    // Retrieve articles listed for ISSN errors in Check Wiki
    retrieveCheckWikiPages(107, tmpWarningPages, null); // Incorrect length
    retrieveCheckWikiPages(108, tmpWarningPages, null); // Wrong checksum

    // Retrieve articles already reported
    retrieveInternalLinks(
        WPCConfigurationString.ISSN_ERRORS_PAGE,
        tmpWarningPages);

    // Fill up the list    
    warningPages.clear();
    warningPages.addAll(tmpWarningPages.values());
    tmpWarningPages.clear();
  }

  /**
   * Display results.
   * 
   * @param stats Statistics.
   * @param startTime Start time.
   * @param errors Errors found.
   */
  private void displayResult(
      Stats stats, long startTime,
      Map<String, List<String>> errors) {
    if (useList) {
      return;
    }

    // Errors
    if (errors != null) {

      // Configuration
      EnumWikipedia wiki = getWikipedia();

      // Compute synthesis
      StringBuilder buffer = new StringBuilder();
      List<String> keys = new ArrayList<String>(errors.keySet());
      Collections.sort(keys);
      for (String key : keys) {
        List<String> values = errors.get(key);
        buffer.append("* ");
        if (values != null) {
          buffer.append(values.size());
          buffer.append(" x ");
        }
        buffer.append("ISSN ");
        buffer.append(key);
        buffer.append(" : ");
        if (values != null) {
          Collections.sort(values);
          int valueNum = 0;
          while (valueNum < values.size()) {
            if (valueNum > 0) {
              buffer.append(", ");
            }
            String value = values.get(valueNum);
            int begin = valueNum;
            while ((valueNum < values.size()) &&
                   (values.get(valueNum).equals(value))) {
              valueNum++;
            }
            if (valueNum > begin + 1) {
              buffer.append(valueNum - begin);
              buffer.append(" x ");
            }
            buffer.append("[[");
            buffer.append(value);
            buffer.append("]]");
          }
        }
        buffer.append("\n");
      }

      // Update synthesis on dedicated page
      WPCConfiguration config = wiki.getConfiguration();
      String pageName = config.getString(WPCConfigurationString.ISSN_ERRORS_PAGE);
      boolean saved = false;
      if ((pageName != null) && (pageName.trim().length() > 0)) {
        boolean updatePage = false;
        if (simulation && (getWindow() != null)) {
          int answer = Utilities.displayYesNoWarning(
              getWindow().getParentComponent(),
              GT._("Do you want to update {0}?", pageName));
          if (answer == JOptionPane.YES_OPTION) {
            updatePage = true;
          }
        } else {
          updatePage = true;
        }

        if (updatePage) {
          try {
            Page page = DataManager.getPage(wiki, pageName, null, null, null);
            API api = APIFactory.getAPI();
            api.retrieveContents(wiki, Collections.singletonList(page), false, false);
            String contents = page.getContents();
            if (contents != null) {
              int begin = -1;
              int end = -1;
              for (ContentsComment comment : page.getAnalysis(contents, true).getComments()) {
                String value = comment.getComment().trim();
                if ("BOT BEGIN".equals(value)) {
                  if (begin < 0) {
                    begin = comment.getEndIndex();
                  }
                } else if ("BOT END".equals(value)) {
                  end = comment.getBeginIndex();
                }
              }
              if ((begin >= 0) && (end > begin)) {
                StringBuilder newText = new StringBuilder();
                newText.append(contents.substring(0, begin));
                newText.append("\n");
                newText.append(buffer.toString());
                newText.append(contents.substring(end));
                api.updatePage(
                    wiki, page, newText.toString(),
                    config.getString(WPCConfigurationString.ISSN_ERRORS_PAGE_COMMENT),
                    true, true, false);
                saved = true;
              }
            }
          } catch (APIException e) {
            // Nothing
          }
        }
      }

      // Display synthesis
      if (!saved && (getWindow() != null)) {
        InformationWindow.createInformationWindow(
            "ISSN", buffer.toString(), false, getWikipedia());
      }
    }

    // Statistics
    displayStats(stats, startTime);
  }
}
