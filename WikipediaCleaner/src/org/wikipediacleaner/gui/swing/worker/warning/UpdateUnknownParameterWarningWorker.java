/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateWarningTools.Stats;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * SwingWorker for updating unknown parameter warning.
 */
public class UpdateUnknownParameterWarningWorker extends UpdateWarningWorker {

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param simulation True if this is a simulation.
   */
  public UpdateUnknownParameterWarningWorker(
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
  public UpdateUnknownParameterWarningWorker(
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
    UpdateUnknownParameterWarningTools tools = new UpdateUnknownParameterWarningTools(
        wiki, this, true, automaticEdit);
    tools.setUsePurge(false);
    try {
      if (!useList) {
        listWarningPages(tools);

        // Ask for confirmation
        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._T(
              "Analysis found {0} articles to check for unknown parameter errors.\n" +
              "Do you want to update the warnings ?",
              Integer.valueOf(warningPages.size()).toString() ));
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
          displayStats(stats, startTime);
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
              int answer = getWindow().displayYesNoWarning(GT._T(
                  "An error occurred when updating unknown parameter warnings. Do you want to continue ?\n\n" +
                  "Error: {0}", e.getMessage()));
              if (answer != JOptionPane.YES_OPTION) {
                return e;
              }
              finish = false;
            }
          }
          if (shouldStop()) {
            Configuration config = Configuration.getConfiguration();
            config.setString(null, ConfigurationValueString.LAST_UNKNOWN_PARAMETER_WARNING, lastTitle);
            displayStats(stats, startTime);
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
      if (warningPages.isEmpty()) {
        Configuration config = Configuration.getConfiguration();
        config.setString(null, ConfigurationValueString.LAST_UNKNOWN_PARAMETER_WARNING, (String) null);
      }
    } catch (APIException e) {
      return e;
    }

    displayStats(stats, startTime);
    return Integer.valueOf(stats.getUpdatedPagesCount());
  }

  /**
   * Generate the list of warning pages.
   * 
   * @param tools Update warning tools.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  protected void listWarningPages(UpdateWarningTools tools) throws APIException {
    Map<String, Page> tmpWarningPages = new HashMap<>();

    // Retrieve talk pages including a warning
    retrieveArticlesWithWarning(
        WPCConfigurationString.UNKNOWN_PARAMETER_WARNING_TEMPLATE,
        tmpWarningPages);

    // Retrieve articles listed for duplicate arguments errors in Check Wiki
    retrieveCheckWikiPages(564, tmpWarningPages, tools); // Unknown parameter

    // Fill up the list
    warningPages.clear();
    warningPages.addAll(tmpWarningPages.values());
    tmpWarningPages.clear();
  }
}
