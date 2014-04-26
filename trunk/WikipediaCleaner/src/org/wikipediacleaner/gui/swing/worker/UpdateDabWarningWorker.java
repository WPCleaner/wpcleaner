/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
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
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateWarningTools.Stats;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * SwingWorker for updating disambiguation warning.
 */
public class UpdateDabWarningWorker extends BasicWorker {

  private final String start;
  private final List<Page> dabWarningPages;
  private final boolean useList;
  private final boolean contentsAvailable;
  private final boolean linksAvailable;
  private final boolean dabInformationAvailable;
  private final boolean automaticEdit;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param start Start at this page.
   */
  public UpdateDabWarningWorker(EnumWikipedia wikipedia, BasicWindow window, String start) {
    super(wikipedia, window);
    this.start = (start != null) ? start.trim() : "";
    this.dabWarningPages = new ArrayList<Page>();
    this.useList = false;
    this.contentsAvailable = false;
    this.linksAvailable = false;
    this.dabInformationAvailable = false;
    this.automaticEdit = true;
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
    super(wikipedia, window);
    this.start = "";
    this.dabWarningPages = new ArrayList<Page>(pages);
    this.useList = true;
    this.contentsAvailable = contentsAvailable;
    this.linksAvailable = linksAvailable;
    this.dabInformationAvailable = dabInformationAvailable;
    this.automaticEdit = automaticEdit;
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
    API api = APIFactory.getAPI();
    int lastCount = 0;
    WikiConfiguration wikiConfiguration = wikipedia.getWikiConfiguration();

    Stats stats = new Stats();
    try {
      if (!useList) {
        // Retrieve talk pages including a disambiguation warning
        String dabWarningTemplateName = configuration.getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
        setText(GT._("Retrieving talk pages including {0}", "{{" + dabWarningTemplateName + "}}"));
        String templateTitle = wikiConfiguration.getPageTitle(
            Namespace.TEMPLATE,
            dabWarningTemplateName);
        Page dabWarningTemplate = DataManager.getPage(
            wikipedia, templateTitle, null, null, null);
        api.retrieveEmbeddedIn(
            wikipedia, dabWarningTemplate,
            configuration.getEncyclopedicTalkNamespaces(),
            false);
        List<Page> dabWarningTalkPages = dabWarningTemplate.getRelatedPages(Page.RelatedPages.EMBEDDED_IN);
  
        // Construct list of articles with disambiguation warning
        setText(GT._("Constructing list of articles with disambiguation warning"));
        HashSet<Page> tmpWarningPages = new HashSet<Page>();
        for (Page dabWarningPage : dabWarningTalkPages) {
          String title = dabWarningPage.getTitle();
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
                  Page page = DataManager.getPage(wikipedia, tmpTitle, null, null, null);
                  if (!tmpWarningPages.contains(page)) {
                    tmpWarningPages.add(page);
                  }
                }
              }
            }
          }
        }
        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._(
              "Analysis found {0} articles with disambiguation warning {1}.\n" +
              "Do you want to update the disambiguation warnings ?",
              new Object[] { Integer.valueOf(tmpWarningPages.size()), "{{" + dabWarningTemplateName + "}}" }));
          if (answer != JOptionPane.YES_OPTION) {
            return Integer.valueOf(0);
          }
        }

        // Sort the list of articles
        dabWarningPages.addAll(tmpWarningPages);
        tmpWarningPages.clear();
        Collections.sort(dabWarningPages, PageComparator.getTitleFirstComparator());
        if (dabWarningPages.isEmpty()) {
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
      while (!dabWarningPages.isEmpty()) {
        // Creating sublist
        int size = Math.min(10, dabWarningPages.size());
        List<Page> sublist = new ArrayList<Page>(size);
        while ((sublist.size() < size) && (dabWarningPages.size() > 0)) {
          Page page = dabWarningPages.remove(0);
          if ((start.length() == 0) || (start.compareTo(page.getTitle()) < 0)) {
            sublist.add(page);
          }
        }
        if (sublist.isEmpty()) {
          displayResult(stats, startTime);
          return Integer.valueOf(stats.getUpdatedPagesCount());
        }

        // Update disambiguation warning
        boolean finish = false;
        while (!finish) {
          finish = true;
          try {
            tools.updateDabWarning(
                sublist, null, null, stats);
            lastTitle = sublist.get(sublist.size() - 1).getTitle();
          } catch (APIException e) {
            if (getWindow() != null) {
              int answer = getWindow().displayYesNoWarning(GT._(
                  "An error occured when updating disambiguation warnings. Do you want to continue ?\n\n" +
                  "Error: {0}", e.getMessage()));
              if (answer != JOptionPane.YES_OPTION) {
                return e;
              }
              finish = false;
            }
          }
          if (shouldStop()) {
            Configuration config = Configuration.getConfiguration();
            config.setString(null, ConfigurationValueString.LAST_DAB_WARNING, lastTitle);
            displayResult(stats, startTime);
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
      if (dabWarningPages.isEmpty()) {
        Configuration config = Configuration.getConfiguration();
        config.setString(null, ConfigurationValueString.LAST_DAB_WARNING, (String) null);
      }
    } catch (APIException e) {
      return e;
    }

    displayResult(stats, startTime);
    return Integer.valueOf(stats.getUpdatedPagesCount());
  }

  /**
   * @param stats Statistics.
   * @param startTime Start time.
   */
  private void displayResult(Stats stats, long startTime) {
    if ((!useList) && (getWindow() != null)) {
      long endTime = System.currentTimeMillis();
      StringBuilder message = new StringBuilder();
      message.append(GT.__(
          "{0} page has been analyzed.",
          "{0} pages have been analyzed.",
          stats.getAnalyedPagesCount(), Integer.toString(stats.getAnalyedPagesCount())));
      message.append("\n");
      message.append(GT.__(
          "Disambiguation warning has been updated in {0} page.",
          "Disambiguation warnings have been updated in {0} pages.",
          stats.getUpdatedPagesCount(), Integer.toString(stats.getUpdatedPagesCount())));
      message.append("\n");
      message.append(GT.__(
          "Disambiguation warning has been removed in {0} page.",
          "Disambiguation wanings have been removed in {0} pages.",
          stats.getRemovedWarningsCount(), Integer.toString(stats.getRemovedWarningsCount())));
      message.append("\n");
      message.append(GT.__(
          "{0} link still needs to be fixed.",
          "{0} links still need to be fixed.",
          stats.getLinksCount(), Integer.toString(stats.getLinksCount())));
      message.append("\n");
      long time = (endTime - startTime) / 1000;
      message.append(GT.__(
          "It took {0} second", "It took {0} seconds", time, Long.toString(time)));
      Utilities.displayInformationMessage(
          getWindow().getParentComponent(), message.toString());
    }
  }
}
