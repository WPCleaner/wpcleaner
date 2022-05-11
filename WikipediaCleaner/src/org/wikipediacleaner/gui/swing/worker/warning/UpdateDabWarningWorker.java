/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.worker.warning.UpdateWarningTools.Stats;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * SwingWorker for updating disambiguation warning.
 */
public class UpdateDabWarningWorker extends UpdateWarningWorker {

  private final boolean linksAvailable;
  private final boolean dabInformationAvailable;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param start Start at this page.
   */
  public UpdateDabWarningWorker(EnumWikipedia wikipedia, BasicWindow window, String start) {
    super(wikipedia, window, start, false);
    this.linksAvailable = false;
    this.dabInformationAvailable = false;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateDabWarningWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<Page> pages, boolean automaticEdit) {
    this(wikipedia, window, pages, false, false, false, automaticEdit);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param linksAvailable True if links are already available in pages.
   * @param dabInformationAvailable True if disambiguation information is already available in pages.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateDabWarningWorker(
      EnumWikipedia wikipedia, BasicWindow window, List<Page> pages,
      boolean contentsAvailable, boolean linksAvailable,
      boolean dabInformationAvailable, boolean automaticEdit) {
    super(wikipedia, window, pages, contentsAvailable, automaticEdit);
    this.linksAvailable = linksAvailable;
    this.dabInformationAvailable = dabInformationAvailable;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    long startTime = System.currentTimeMillis();
    EnumWikipedia wikipedia = getWikipedia();
    WPCConfiguration configuration = wikipedia.getConfiguration();
    setText(GT._T("Retrieving MediaWiki API"));
    int lastCount = 0;

    Stats stats = new Stats();
    UpdateDabWarningTools tools = new UpdateDabWarningTools(wikipedia, this, true, automaticEdit);
    try {
      if (!useList) {
        listWarningPages(tools);

        // Ask for confirmation
        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._T(
              "Analysis found {0} articles with disambiguation warning {1}.\n" +
              "Do you want to update the disambiguation warnings ?",
              new Object[] {
                  Integer.valueOf(warningPages.size()),
                  TemplateBuilder.from(configuration.getString(WPCConfigurationString.DAB_WARNING_TEMPLATE)).toString() }));
          if (answer != JOptionPane.YES_OPTION) {
            return Integer.valueOf(0);
          }
        }

        // Sort the list of articles (trying a temporary ArrayList for performance)
        if (warningPages.isEmpty()) {
          return Integer.valueOf(0);
        }
        List<Page> tmpWarningPages = new ArrayList<>(warningPages);
        Collections.sort(tmpWarningPages, PageComparator.getTitleFirstComparator());
        warningPages.clear();
        warningPages.addAll(tmpWarningPages);
      }

      // Working with sublists
      tools.setContentsAvailable(contentsAvailable);
      tools.setLinksAvailable(linksAvailable);
      tools.setDabInformationAvailable(dabInformationAvailable);
      if (!useList) {
        setText(GT._T("Retrieving disambiguation pages"));
        tools.preloadDabPages();
      }
      String lastTitle = null;
      int countUnsaved = 0;
      while (!warningPages.isEmpty()) {
        // Creating sublist
        List<Page> sublist = tools.extractSublist(warningPages, 10, false);
        if (sublist.isEmpty()) {
          displayStats(stats, startTime);
          return Integer.valueOf(stats.getUpdatedPagesCount());
        }
        countUnsaved += sublist.size();

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
                  "An error occurred when updating disambiguation warnings. Do you want to continue ?\n\n" +
                  "Error: {0}", e.getMessage()));
              if (answer != JOptionPane.YES_OPTION) {
                return e;
              }
              finish = false;
            }
          }
          if (shouldStop() || (countUnsaved > 1000)) {
            Configuration config = Configuration.getConfiguration();
            config.setString(null, ConfigurationValueString.LAST_DAB_WARNING, lastTitle);
            countUnsaved = 0;
          }
          if (shouldStop()) {
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
        config.setString(null, ConfigurationValueString.LAST_DAB_WARNING, (String) null);
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
        WPCConfigurationString.DAB_WARNING_TEMPLATE,
        tmpWarningPages);

    // Fill up the list    
    warningPages.clear();
    warningPages.addAll(tmpWarningPages.values());
    tmpWarningPages.clear();
  }
}
