/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateWarningTools.Stats;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * SwingWorker for updating duplicate arguments warning.
 */
public class UpdateDuplicateArgsWarningWorker extends BasicWorker {

  private final List<Page> warningPages;
  private final boolean useList;
  private final boolean contentsAvailable;
  private final boolean simulation;
  private final boolean automaticEdit;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param simulation True if this is a simulation.
   */
  public UpdateDuplicateArgsWarningWorker(
      EnumWikipedia wiki, BasicWindow window,
      boolean simulation) {
    super(wiki, window);
    this.warningPages = new ArrayList<Page>();
    this.useList = false;
    this.contentsAvailable = false;
    this.simulation = simulation;
    this.automaticEdit = true;
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateDuplicateArgsWarningWorker(
      EnumWikipedia wiki, BasicWindow window, List<Page> pages,
      boolean contentsAvailable, boolean automaticEdit) {
    super(wiki, window);
    this.warningPages = new ArrayList<Page>(pages);
    this.useList = true;
    this.contentsAvailable = contentsAvailable;
    this.simulation = false;
    this.automaticEdit = automaticEdit;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    long startTime = System.currentTimeMillis();
    EnumWikipedia wiki = getWikipedia();
    WPCConfiguration configuration = wiki.getConfiguration();

    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();
    int lastCount = 0;
    WikiConfiguration wikiConfiguration = wiki.getWikiConfiguration();

    Stats stats = new Stats();
    Map<String, List<String>> errors = null;
    try {
      if (!useList) {
        warningPages.clear();

        // Retrieve talk pages including a warning
        String warningTemplateName = configuration.getString(
            WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE);
        if (warningTemplateName != null) {
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
          warningPages.addAll(warningTemplate.getRelatedPages(Page.RelatedPages.EMBEDDED_IN));
        }

        // Retrieve articles listed for duplicate arguments errors in Check Wiki
        retrieveCheckWikiPages(524, warningPages); // Duplicate template arguments

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
          Page page = DataManager.getPage(wiki, title, null, null, null);
          if (encyclopedicNamespaces.contains(page.getNamespace()) &&
              !tmpWarningPages.contains(page)) {
            tmpWarningPages.add(page);
          }
        }

        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._(
              "Analysis found {0} articles to check for duplicate arguments errors.\n" +
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
      UpdateDuplicateArgsWarningTools tools = new UpdateDuplicateArgsWarningTools(
          wiki, this, true, automaticEdit);
      tools.setContentsAvailable(contentsAvailable);
      tools.prepareErrorsMap();
      if (simulation) {
        tools.setSimulation(true);
      }
      String lastTitle = null;
      while (!warningPages.isEmpty()) {
        // Creating sublist
        int size = Math.min(10, warningPages.size());
        List<Page> sublist = new ArrayList<Page>(size);
        while ((sublist.size() < size) && (warningPages.size() > 0)) {
          Page page = warningPages.remove(0);
          sublist.add(page);
        }
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
            tools.updateWarning(sublist, null, null, stats);
            lastTitle = sublist.get(sublist.size() - 1).getTitle();
          } catch (APIException e) {
            if (getWindow() != null) {
              int answer = getWindow().displayYesNoWarning(GT._(
                  "An error occurred when updating duplicate arguments warnings. Do you want to continue ?\n\n" +
                  "Error: {0}", e.getMessage()));
              if (answer != JOptionPane.YES_OPTION) {
                return e;
              }
              finish = false;
            }
          }
          if (shouldStop()) {
            Configuration config = Configuration.getConfiguration();
            config.setString(null, ConfigurationValueString.LAST_DUPLICATE_ARGS_WARNING, lastTitle);
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
        config.setString(null, ConfigurationValueString.LAST_DUPLICATE_ARGS_WARNING, (String) null);
      }
    } catch (APIException e) {
      return e;
    }

    displayResult(stats, startTime, errors);
    return Integer.valueOf(stats.getUpdatedPagesCount());
  }

  /**
   * Retrieve pages for a given error number.
   * 
   * @param errorNumber Error number.
   * @param pages List of pages to complete.
   */
  private void retrieveCheckWikiPages(int errorNumber, List<Page> pages) {
    CheckWiki cw = APIFactory.getCheckWiki();
    EnumWikipedia wiki = getWikipedia();
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, errorNumber);
    List<CheckError> errors = new ArrayList<CheckError>();
    try {
      cw.retrievePages(algorithm, 10000, wiki, errors);
      for (CheckError error: errors) {
        for (int pageNum = 0; pageNum < error.getPageCount(); pageNum++) {
          pages.add(error.getPage(pageNum));
        }
      }
    } catch (APIException e) {
      // Nothing
    }
  }

  /**
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
      List<String[]> issnSearchEngines = wiki.getConfiguration().getStringArrayList(
          WPCConfigurationStringList.ISSN_SEARCH_ENGINES);
      String issnUrl = null;
      if ((issnSearchEngines != null) &&
          !issnSearchEngines.isEmpty()) {
        String[] issnSearchEngine0 = issnSearchEngines.get(0);
        if ((issnSearchEngine0 != null) && (issnSearchEngine0.length > 1)) {
          issnUrl = issnSearchEngine0[1];
        }
      }

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
        buffer.append("ISBN ");
        buffer.append(key);
        if (issnUrl != null) {
          String clean = key.replaceAll("\\&\\#x3D\\;", "=");
          clean = PageElementISBN.cleanISBN(clean);
          if (clean.length() == 8) {
            buffer.append(" ([");
            buffer.append(MessageFormat.format(issnUrl, clean));
            buffer.append(" ISSN?])");
          }
        }
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
      String pageName = config.getString(WPCConfigurationString.ISBN_ERRORS_PAGE);
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
              for (PageElementComment comment : page.getAnalysis(contents, true).getComments()) {
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
                    wiki.formatComment(
                        config.getString(WPCConfigurationString.ISBN_ERRORS_PAGE_COMMENT),
                        true),
                    false);
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
            "ISBN", buffer.toString(), false, getWikipedia());
      }
    }

    // Statistics
    if (getWindow() != null) {
      long endTime = System.currentTimeMillis();
      StringBuilder message = new StringBuilder();
      message.append(GT.__(
          "{0} page has been analyzed.",
          "{0} pages have been analyzed.",
          stats.getAnalyedPagesCount(), Integer.toString(stats.getAnalyedPagesCount())));
      message.append("\n");
      message.append(GT.__(
          "Warning has been updated on {0} page.",
          "Warnings have been updated on {0} pages.",
          stats.getUpdatedPagesCount(), Integer.toString(stats.getUpdatedPagesCount())));
      message.append("\n");
      message.append(GT.__(
          "Warning has been removed on {0} page.",
          "Warnings have been removed on {0} pages.",
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
