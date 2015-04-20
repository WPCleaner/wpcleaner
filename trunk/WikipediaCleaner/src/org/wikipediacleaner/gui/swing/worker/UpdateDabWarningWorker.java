/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.worker.UpdateWarningTools.Stats;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * SwingWorker for updating disambiguation warning.
 */
public class UpdateDabWarningWorker extends UpdateWarningWorker {

  private final String start;
  private final boolean linksAvailable;
  private final boolean dabInformationAvailable;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param start Start at this page.
   */
  public UpdateDabWarningWorker(EnumWikipedia wikipedia, BasicWindow window, String start) {
    super(wikipedia, window, false);
    this.start = (start != null) ? start.trim() : "";
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
    this.start = "";
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
    setText(GT._("Retrieving MediaWiki API"));
    int lastCount = 0;

    Stats stats = new Stats();
    try {
      if (!useList) {
        listWarningPages();

        // Ask for confirmation
        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._(
              "Analysis found {0} articles with disambiguation warning {1}.\n" +
              "Do you want to update the disambiguation warnings ?",
              new Object[] {
                  Integer.valueOf(warningPages.size()),
                  "{{" + configuration.getString(WPCConfigurationString.DAB_WARNING_TEMPLATE) + "}}" }));
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
      UpdateDabWarningTools tools = new UpdateDabWarningTools(wikipedia, this, true, automaticEdit);
      tools.setContentsAvailable(contentsAvailable);
      tools.setLinksAvailable(linksAvailable);
      tools.setDabInformationAvailable(dabInformationAvailable);
      if (!useList) {
        setText(GT._("Retrieving disambiguation pages"));
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
            tools.updateWarning(sublist, null, null, stats);
            lastTitle = sublist.get(sublist.size() - 1).getTitle();
          } catch (APIException e) {
            if (getWindow() != null) {
              int answer = getWindow().displayYesNoWarning(GT._(
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
   */
  @Override
  protected void listWarningPages() throws APIException {
    EnumWikipedia wiki = getWikipedia();
    WPCConfiguration configuration = wiki.getConfiguration();
    WikiConfiguration wikiConfiguration = wiki.getWikiConfiguration();
    API api = APIFactory.getAPI();

    // Retrieve talk pages including a warning
    String warningTemplateName = configuration.getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
    setText(GT._("Retrieving talk pages including {0}", "{{" + warningTemplateName + "}}"));
    String templateTitle = wikiConfiguration.getPageTitle(
        Namespace.TEMPLATE,
        warningTemplateName);
    Page warningTemplate = DataManager.getPage(
        wiki, templateTitle, null, null, null);
    api.retrieveEmbeddedIn(
        wiki, warningTemplate,
        configuration.getEncyclopedicTalkNamespaces(),
        false);
    List<Page> warningTalkPages = warningTemplate.getRelatedPages(Page.RelatedPages.EMBEDDED_IN);

    // Construct list of articles with warning
    setText(GT._("Constructing list of articles with warning"));
    HashSet<Page> tmpWarningPages = new HashSet<Page>();
    for (Page warningPage : warningTalkPages) {
      String title = warningPage.getTitle();
      String todoSubpage = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
      if (title.endsWith("/" + todoSubpage)) {
        title = title.substring(0, title.length() - 1 - todoSubpage.length());
      }
      int colonIndex = title.indexOf(':');
      if (colonIndex >= 0) {
        for (Integer namespace : configuration.getEncyclopedicTalkNamespaces()) {
          Namespace namespaceTalk = wikiConfiguration.getNamespace(namespace);
          if ((namespaceTalk != null) &&
              (namespaceTalk.isPossibleName(title.substring(0, colonIndex)))) {
            String tmpTitle = title.substring(colonIndex + 1);
            if (namespace != Namespace.MAIN_TALK) {
              tmpTitle = wikiConfiguration.getPageTitle(namespace - 1, tmpTitle);
            }
            if ((start.length() == 0) || (start.compareTo(tmpTitle) < 0)) {
              Page page = DataManager.getPage(wiki, tmpTitle, null, null, null);
              if (!tmpWarningPages.contains(page)) {
                tmpWarningPages.add(page);
              }
            }
          }
        }
      }
    }

    // Fill up the list    
    warningPages.addAll(tmpWarningPages);
    tmpWarningPages.clear();
  }
}
