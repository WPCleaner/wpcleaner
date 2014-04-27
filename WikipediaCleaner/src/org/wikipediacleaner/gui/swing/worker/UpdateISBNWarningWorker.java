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
 * SwingWorker for updating ISBN warning.
 */
public class UpdateISBNWarningWorker extends BasicWorker {

  private final String start;
  private final List<Page> warningPages;
  private final boolean useList;
  private final boolean contentsAvailable;
  private final boolean automaticEdit;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param start Start at this page.
   */
  public UpdateISBNWarningWorker(EnumWikipedia wikipedia, BasicWindow window, String start) {
    super(wikipedia, window);
    this.start = (start != null) ? start.trim() : "";
    this.warningPages = new ArrayList<Page>();
    this.useList = false;
    this.contentsAvailable = false;
    this.automaticEdit = true;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateISBNWarningWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<Page> pages, boolean automaticEdit) {
    this(wikipedia, window, pages, false, automaticEdit);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateISBNWarningWorker(
      EnumWikipedia wikipedia, BasicWindow window, List<Page> pages,
      boolean contentsAvailable, boolean automaticEdit) {
    super(wikipedia, window);
    this.start = "";
    this.warningPages = new ArrayList<Page>(pages);
    this.useList = true;
    this.contentsAvailable = contentsAvailable;
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
        warningPages.clear();

        // Retrieve talk pages including a warning
        String warningTemplateName = configuration.getString(WPCConfigurationString.ISBN_WARNING_TEMPLATE);
        setText(GT._("Retrieving talk pages including {0}", "{{" + warningTemplateName + "}}"));
        String templateTitle = wikiConfiguration.getPageTitle(
            Namespace.TEMPLATE,
            warningTemplateName);
        Page warningTemplate = DataManager.getPage(
            wikipedia, templateTitle, null, null, null);
        api.retrieveEmbeddedIn(
            wikipedia, warningTemplate,
            configuration.getEncyclopedicTalkNamespaces(),
            false);
        warningPages.addAll(warningTemplate.getRelatedPages(Page.RelatedPages.EMBEDDED_IN));
  
        // Construct list of articles with warning
        setText(GT._("Constructing list of articles with warning"));
        HashSet<Page> tmpWarningPages = new HashSet<Page>();
        List<Integer> encyclopedicNamespaces = configuration.getEncyclopedicNamespaces();
        for (Page warningPage : warningPages) {

          // Get article page for talks pages and to do sub-pages
          String title = warningPage.getTitle();
          if (!warningPage.isArticle()) {
            String todoSubpage = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
            if (title.endsWith("/" + todoSubpage)) {
              title = title.substring(0, title.length() - 1 - todoSubpage.length());
            }
            Integer namespace = warningPage.getNamespace();
            if (namespace != null) {
              Namespace namespaceTalk = wikiConfiguration.getNamespace(namespace.intValue());
              if (namespaceTalk != null) {
                int colonIndex = title.indexOf(':');
                if (colonIndex >= 0) {
                  title = title.substring(colonIndex + 1);
                }
                if (namespace != Namespace.MAIN_TALK) {
                  title = wikiConfiguration.getPageTitle(namespace - 1, title);
                }
              }
            }
          }

          // Add article to the list
          if ((start.length() == 0) || (start.compareTo(title) < 0)) {
            Page page = DataManager.getPage(wikipedia, title, null, null, null);
            if (encyclopedicNamespaces.contains(page.getNamespace()) &&
                !tmpWarningPages.contains(page)) {
              tmpWarningPages.add(page);
            }
          }
        }

        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._(
              "Analysis found {0} articles to check for ISBN errors.\n" +
              "Do you want to update the warnings ?",
              Integer.valueOf(tmpWarningPages.size()).toString() ));
          if (answer != JOptionPane.YES_OPTION) {
            return Integer.valueOf(0);
          }
        }

        // Sort the list of articles
        warningPages.clear();
        warningPages.addAll(tmpWarningPages);
        tmpWarningPages.clear();
        Collections.sort(warningPages, PageComparator.getTitleFirstComparator());
        if (warningPages.isEmpty()) {
          return Integer.valueOf(0);
        }
      }

      // Working with sublists
      UpdateISBNWarningTools tools = new UpdateISBNWarningTools(wikipedia, this, true, automaticEdit);
      tools.setContentsAvailable(contentsAvailable);
      String lastTitle = null;
      while (!warningPages.isEmpty()) {
        // Creating sublist
        int size = Math.min(10, warningPages.size());
        List<Page> sublist = new ArrayList<Page>(size);
        while ((sublist.size() < size) && (warningPages.size() > 0)) {
          Page page = warningPages.remove(0);
          if ((start.length() == 0) || (start.compareTo(page.getTitle()) < 0)) {
            sublist.add(page);
          }
        }
        if (sublist.isEmpty()) {
          displayResult(stats, startTime);
          return Integer.valueOf(stats.getUpdatedPagesCount());
        }

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
            config.setString(null, ConfigurationValueString.LAST_ISBN_WARNING, lastTitle);
            displayResult(stats, startTime);
            return Integer.valueOf(stats.getUpdatedPagesCount());
          }
        }

        if (stats.getUpdatedPagesCount() > lastCount) {
          lastCount = stats.getUpdatedPagesCount();
          if (getWindow() != null) {
            int answer = getWindow().displayYesNoWarning(
                "This feature is currently under development, please check the modification.\n" +
                "Do you want to continue ?");
            if (answer != JOptionPane.YES_OPTION) {
              return Integer.valueOf(lastCount);
            }
          } else {
            return Integer.valueOf(lastCount);
          }
        }
      }
      if (warningPages.isEmpty()) {
        Configuration config = Configuration.getConfiguration();
        config.setString(null, ConfigurationValueString.LAST_ISBN_WARNING, (String) null);
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
          "Warning has been updated in {0} page.",
          "Warnings have been updated in {0} pages.",
          stats.getUpdatedPagesCount(), Integer.toString(stats.getUpdatedPagesCount())));
      message.append("\n");
      message.append(GT.__(
          "Warning has been removed in {0} page.",
          "Warnings have been removed in {0} pages.",
          stats.getRemovedWarningsCount(), Integer.toString(stats.getRemovedWarningsCount())));
      message.append("\n");
      message.append(GT.__(
          "{0} still needs to be fixed.",
          "{0} still need to be fixed.",
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
