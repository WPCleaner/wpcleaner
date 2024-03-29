/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.configuration.WikiConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Swing worker for updating various warnings.
 */
public abstract class UpdateWarningWorker extends BasicWorker {

  /** List of pages. */
  protected final LinkedList<Page> warningPages;

  /** True if the list of pages should be used. */
  protected final boolean useList;

  /** Starting point. */
  protected final String start;

  /** True if the contents is already available in articles. */
  protected final boolean contentsAvailable;

  /** True if edits should be considered automatic. */
  protected final boolean automaticEdit;

  /** True if this is a simulation. */
  protected final boolean simulation;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param start Starting point.
   * @param simulation True if this is a simulation.
   */
  public UpdateWarningWorker(
      EnumWikipedia wiki, BasicWindow window,
      String start, boolean simulation) {
    super(wiki, window);
    this.warningPages = new LinkedList<>();
    this.useList = false;
    this.start = (start != null) ? start.trim() : "";
    this.contentsAvailable = false;
    this.automaticEdit = true;
    this.simulation = simulation;
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateWarningWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<Page> pages, boolean contentsAvailable,
      boolean automaticEdit) {
    super(wiki, window);
    this.warningPages = new LinkedList<>(pages);
    this.useList = true;
    this.start = "";
    this.contentsAvailable = contentsAvailable;
    this.automaticEdit = automaticEdit;
    this.simulation = false;
  }

  /**
   * Generate the list of warning pages.
   * 
   * @param tools Update warning tools.
   * @throws APIException Exception thrown by the API.
   */
  protected abstract void listWarningPages(UpdateWarningTools tools) throws APIException;

  /**
   * Retrieve pages with a warning on their talk page.
   * 
   * @param templateNameProperty Property for the name of the warning template.
   * @param pages Map of (title,page) to complete.
   * @throws APIException Exception thrown by the API.
   */
  protected void retrieveArticlesWithWarning(
      WPCConfigurationString templateNameProperty,
      Map<String, Page> pages) throws APIException {
    EnumWikipedia wiki = getWikipedia();
    WPCConfiguration configuration = wiki.getConfiguration();
    WikiConfiguration wikiConfiguration = wiki.getWikiConfiguration();
    API api = APIFactory.getAPI();

    // Retrieve talk pages including a warning
    String warningTemplateName = configuration.getString(templateNameProperty);
    if (warningTemplateName != null) {
      setText(GT._T("Retrieving talk pages including {0}", TemplateBuilder.from(warningTemplateName).toString()));
      String templateTitle = wikiConfiguration.getPageTitle(
          Namespace.TEMPLATE,
          warningTemplateName);
      Page warningTemplate = DataManager.createSimplePage(
          wiki, templateTitle, null, null, null);
      api.retrieveEmbeddedIn(
          wiki, warningTemplate,
          configuration.getEncyclopedicTalkNamespaces(),
          false);

      // Convert them to article pages
      setText(GT._T("Constructing list of articles with warning"));
      List<Page> talkPages = warningTemplate.getRelatedPages(Page.RelatedPages.EMBEDDED_IN);
      if (talkPages != null) {
        for (Page talkPage : talkPages) {
          Page page=  null;
          if (talkPage.isArticle()) {
            page = talkPage;
          } else {
            String title = talkPage.getTitle();
            String todoSubpage = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
            if (title.endsWith("/" + todoSubpage)) {
              title = title.substring(0, title.length() - 1 - todoSubpage.length());
            }
            Integer namespace = talkPage.getNamespace();
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
            page = DataManager.createSimplePage(wiki, title, null, null, null);
          }
          addPage(page, pages);
        }
      }
    }
  }

  /**
   * Retrieve pages for a given error number.
   * 
   * @param errorNumber Error number.
   * @param pages Map of (title,page) to complete.
   * @param tools Update warning tools if the pages should be added as articles.
   */
  protected void retrieveCheckWikiPages(
      int errorNumber,
      Map<String, Page> pages,
      UpdateWarningTools tools) {
    CheckWiki cw = APIFactory.getCheckWiki();
    EnumWikipedia wiki = getWikipedia();
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, errorNumber);
    List<AlgorithmError> errors = new ArrayList<>();
    try {
      cw.retrievePages(algorithm, 100000, wiki, errors);
      for (AlgorithmError error: errors) {
        for (int pageNum = 0; pageNum < error.getPageCount(); pageNum++) {
          Page page = error.getPage(pageNum);
          addPage(page, pages);
          if (tools != null) {
            tools.addArticle(page.getTitle());
          }
        }
      }
    } catch (APIException e) {
      // Nothing
    }
  }

  /**
   * Retrieve category members
   * 
   * @param categoryNamesProperty Property for the name of the categories.
   * @param pages Map of (title,page) to complete.
   * @throws APIException Exception thrown by the API.
   */
  protected void retrieveCategoryMembers(
      WPCConfigurationStringList categoryNamesProperty,
      Map<String, Page> pages) throws APIException {
    EnumWikipedia wiki = getWikipedia();
    WikiConfiguration wikiConfiguration = wiki.getWikiConfiguration();
    WPCConfiguration configuration = wiki.getConfiguration();
    API api = APIFactory.getAPI();
    List<String> categories = configuration.getStringList(categoryNamesProperty);
    if (categories != null) {
      for (String category : categories) {
        String categoryTitle = wikiConfiguration.getPageTitle(Namespace.CATEGORY, category);
        Page categoryPage = DataManager.createSimplePage(
            wiki, categoryTitle, null, null, Namespace.CATEGORY);
        api.retrieveCategoryMembers(wiki, categoryPage, 0, false, Integer.MAX_VALUE);
        List<Page> categoryMembers = categoryPage.getRelatedPages(
            Page.RelatedPages.CATEGORY_MEMBERS);
        if (categoryMembers != null) {
          for (Page page : categoryMembers) {
            addPage(page, pages);
          }
        }
      }
    }
  }

  /**
   * Retrieve internal links in a page.
   * 
   * @param pageNameProperty Property for the name of the page.
   * @param pages Map of (title,page) to complete.
   * @throws APIException Exception thrown by the API.
   */
  protected void retrieveInternalLinks(
      WPCConfigurationString pageNameProperty,
      Map<String, Page> pages) throws APIException {
    EnumWikipedia wiki = getWikipedia();
    WPCConfiguration configuration = wiki.getConfiguration();
    API api = APIFactory.getAPI();
    String pageName = configuration.getString(pageNameProperty);
    if (pageName != null) {
      Page page = DataManager.createSimplePage(wiki, pageName, null, null, null);
      api.retrieveLinks(wiki, page, Namespace.MAIN, null, false, false);
      List<Page> links = page.getLinks();
      if (links != null) {
        for (Page link : links) {
          addPage(link, pages);
        }
      }
    }
  }

  /**
   * Retrieve internal links in a page.
   * 
   * @param pageNameProperty Property for the name of the page.
   * @param pages Map of (title,page) to complete.
   * @throws APIException Exception thrown by the API.
   */
  protected void retrieveInternalLinks(
      WPCConfigurationStringList pageNameProperty,
      Map<String, Page> pages) throws APIException {
    EnumWikipedia wiki = getWikipedia();
    WPCConfiguration configuration = wiki.getConfiguration();
    API api = APIFactory.getAPI();
    List<String> pageNames = configuration.getStringList(pageNameProperty);
    if (pageNames != null) {
      for (String pageName : pageNames) {
        Page page = DataManager.createSimplePage(wiki, pageName, null, null, null);
        api.retrieveLinks(wiki, page, Namespace.MAIN, null, false, false);
        List<Page> links = page.getLinks();
        if (links != null) {
          for (Page link : links) {
            addPage(link, pages);
          }
        }
      }
    }
  }

  /**
   * Add a page to the list of pages.
   * 
   * @param page Page.
   * @param pages Map of (title,page) to complete.
   */
  private void addPage(Page page, Map<String, Page> pages) {
    if ((page == null) || (pages == null)) {
      return;
    }
    String title = page.getTitle();
    if (title == null) {
      return;
    }
    if ((start.length() == 0) || (start.compareTo(title) <= 0)) {
      if (!pages.containsKey(title)) {
        pages.put(title, page);
      }
    }
  }

  /**
   * Display statistics.
   * 
   * @param stats Statistics.
   * @param startTime Start time.
   */
  protected void displayStats(
      WarningStats stats, long startTime) {
    if (useList) {
      return;
    }
    WarningStats.displayStats(getWindow(), stats, startTime);
  }
}
