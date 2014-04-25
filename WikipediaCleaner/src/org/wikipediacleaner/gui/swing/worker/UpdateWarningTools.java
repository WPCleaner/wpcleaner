/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;


/**
 * Tools for updating disambiguation warnings.
 */
public abstract class UpdateWarningTools {

  /** Wiki. */
  protected final EnumWikipedia wiki;

  /** Wiki configuration. */
  protected final WPCConfiguration configuration;

  /** Worker. */
  protected final BasicWorker worker;

  /** Window. */
  protected final BasicWindow window;

  /** True for allowing warning creation.  */
  protected final boolean createWarning;

  /** True if this is an automatic edit. */
  protected final boolean automaticEdit;

  /** Force use of section 0 in the talk page. */
  protected final boolean section0;

  /** MediaWiki API. */
  protected final API api;

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  protected UpdateWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    this.wiki = wiki;
    this.configuration = wiki.getConfiguration();
    this.worker = worker;
    this.window = window;
    this.createWarning = createWarning;
    this.automaticEdit = automaticEdit;
    this.section0 = useSection0();
    this.api = APIFactory.getAPI();
  }

  /**
   * @param talkPage Talk page
   * @param contents Talk page contents.
   * @return Template containing a list to the todo subpage.
   */
  protected PageElementTemplate getExistingTemplateTodoLink(Page talkPage, String contents) {
    PageElementTemplate templateTodoLink = null;
    List<String> todoLinkTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_LINK_TEMPLATES);
    if (todoLinkTemplates != null) {
      PageAnalysis analysis = talkPage.getAnalysis(contents, true);
      for (String todoLink : todoLinkTemplates) {
        List<PageElementTemplate> templates = analysis.getTemplates(todoLink);
        if ((templates != null) && (templates.size() > 0)) {
          templateTodoLink = templates.get(0);
        }
      }
    }
    return templateTodoLink;
  }

  /**
   * Tell if the template should be modified.
   * 
   * @param params List of parameters for the warning template.
   * @param template Template.
   * @return True if the template should be modified.
   */
  protected boolean isModified(Collection<String> params, PageElementTemplate template) {
    // Check that parameters in template are still useful
    int paramNum = 1;
    while (template.getParameterValue(Integer.toString(paramNum)) != null) {
      String param = template.getParameterValue(Integer.toString(paramNum)).trim();
      if (!params.contains(param)) {
        return true;
      }
      paramNum++;
    }

    // Check that current parameters are already in the template
    for (String param : params) {
      boolean found = false;
      paramNum = 1;
      while ((found == false) && (template.getParameterValue(Integer.toString(paramNum)) != null)) {
        if (param.equals(template.getParameterValue(Integer.toString(paramNum)))) {
          found = true;
        }
        paramNum++;
      }
      if (!found) {
        return true;
      }
    }

    return false;
  }

  /**
   * Add a warning in a text.
   * 
   * @param talkText Text in which the warning should be added.
   * @param pageRevId Page revision id.
   * @param params List of parameters for the warning template.
   */
  protected void addWarning(
      StringBuilder talkText,
      Integer pageRevId, Collection<String> params) {
    talkText.append("{{ ");
    talkText.append(configuration.getString(getWarningTemplate()));
    if (pageRevId != null) {
      talkText.append(" | revisionid=");
      talkText.append(pageRevId);
    }
    for (String param : params) {
      talkText.append(" | ");
      talkText.append(param);
    }
    talkText.append(" }} -- ~~~~~");
    String comment = configuration.getString(getWarningTemplateComment());
    if (comment != null) {
      talkText.append(" <!-- ");
      talkText.append(comment);
      talkText.append(" -->");
    }
  }

  // ==========================================================================
  // Page analysis
  // ==========================================================================

  /**
   * @param analysis Page analysis.
   * @return First warning template in the page.
   */
  protected final PageElementTemplate getFirstWarningTemplate(PageAnalysis analysis) {
    PageElementTemplate template = null;
    if (analysis != null) {
      List<PageElementTemplate> templates = analysis.getTemplates(
          configuration.getString(getWarningTemplate()));
      if ((templates != null) && (!templates.isEmpty())) {
        template = templates.get(0);
      }
    }
    return template;
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  protected abstract WPCConfigurationString getWarningTemplate();

  /**
   * @return Configuration parameter for the warning template comment.
   */
  protected abstract WPCConfigurationString getWarningTemplateComment();

  /**
   * @return True if section 0 of the talk page should be used.
   */
  protected abstract boolean useSection0();

  // ==========================================================================
  // Utility methods
  // ==========================================================================

  /**
   * Update a talk page on Wiki.
   * 
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @return Result of the command.
   * @throws APIException
   */
  protected QueryResult updateTalkPage(
      Page page, String newContents,
      String comment) throws APIException {
    if (section0) {
      return updateSection(page, comment, 0, newContents, false);
    }
    return updatePage(page, newContents, comment, false);
  }

  /**
   * Update a page on Wiki.
   * 
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  protected QueryResult updatePage(
      Page page,
      String newContents, String comment,
      boolean forceWatch) throws APIException {
    return api.updatePage(wiki, page, newContents, comment, forceWatch);
  }

  /**
   * Update a section in a page.
   * 
   * @param page Page.
   * @param title Title of the new section.
   * @param section Section. 
   * @param contents Contents.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  protected QueryResult updateSection(
      Page page, String title, int section,
      String contents, boolean forceWatch) throws APIException {
    return api.updateSection(wiki, page, title, section, contents, forceWatch);
  }

  /**
   * @return True if the analyze should stop.
   */
  protected boolean shouldStop() {
    if ((window == null) ||
        (window.getParentComponent() == null) ||
        (window.getParentComponent().isDisplayable() == false)) {
      return true;
    }
    return false;
  }

  /**
   * Display text.
   * 
   * @param text Text to display.
   */
  protected void setText(String text) {
    if (worker != null) {
      worker.setText(text);
    }
  }

  // ==========================================================================
  // Statistics
  // ==========================================================================

  /** Bean for holding statistics. */
  public static class Stats {

    /**
     * Count of analyzed pages.
     */
    private int analyzedPagesCount;

    /**
     * List of updated pages.
     */
    private List<Page> updatedPages;

    /**
     * Count of disambiguation warning that have been removed.
     */
    private int removedWarningsCount;

    /**
     * Count of links to disambiguation pages.
     */
    private int linksCount;

    public Stats() {
      analyzedPagesCount = 0;
      updatedPages = new ArrayList<Page>();
    }

    void addAnalyzedPage(Page page) {
      if (page != null) {
        analyzedPagesCount++;
      }
    }

    public int getAnalyedPagesCount() {
      return analyzedPagesCount;
    }

    void addUpdatedPage(Page page) {
      if (page != null) {
        updatedPages.add(page);
      }
    }

    public List<Page> getUpdatedPages() {
      return updatedPages;
    }

    public int getUpdatedPagesCount() {
      return (updatedPages != null) ? updatedPages.size() : 0;
    }

    public int getRemovedWarningsCount() {
      return removedWarningsCount;
    }

    void addRemovedWarning(Page page) {
      if (page != null) {
        removedWarningsCount++;
      }
    }

    public int getLinksCount() {
      return linksCount;
    }

    void addLinks(Page page, int count) {
      if (page != null) {
        linksCount += count;
      }
    }
  }
}
