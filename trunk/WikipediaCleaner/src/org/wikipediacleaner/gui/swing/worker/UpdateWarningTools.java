/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.api.data.Section;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Tools for updating disambiguation warnings.
 */
public abstract class UpdateWarningTools {

  private final static Log log = LogFactory.getLog(UpdateWarningTools.class);

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
   * @return Configuration parameter for the title for a message for a new article.
   */
  protected WPCConfigurationString getMessageTitleNewArticle() {
    return null;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  protected WPCConfigurationString getMessageTitleNewArticleModified() {
    return null;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  protected WPCConfigurationString getMessageTitleNewArticleModifier() {
    return null;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  protected WPCConfigurationString getMessageTemplateNewArticle() {
    return null;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  protected WPCConfigurationString getMessageTemplateNewArticleModified() {
    return null;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  protected WPCConfigurationString getMessageTemplateNewArticleModifier() {
    return null;
  }

  /**
   * @return True if section 0 of the talk page should be used.
   */
  protected abstract boolean useSection0();

  /**
   * @return Comment when warning is removed.
   */
  protected abstract String getWarningCommentDone();

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  protected abstract String getWarningComment(Collection<String> elements);

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  protected abstract String getMessageRemoveWarning(String title);

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  protected abstract String getMessageUpdateWarning(String title);

  // ==========================================================================
  // Contributors management
  // ==========================================================================

  /**
   * Inform page contributors.
   * 
   * @param analysis Page analysis.
   * @param msgElements Message elements.
   * @param creator User who has created the page.
   * @param modifiers Other contributors to the page.
   */
  // TODO: private ?
  protected void informContributors(
      PageAnalysis analysis,
      Collection<String> msgElements,
      String creator,
      List<String> modifiers) {
    if (analysis == null) {
      return;
    }

    if (creator != null) {
      if ((modifiers == null) || (modifiers.isEmpty())) {
        addMessage(
            analysis, msgElements, creator,
            getMessageTitleNewArticle(),
            getMessageTemplateNewArticle());
      } else {
        addMessage(
            analysis, msgElements, creator,
            getMessageTitleNewArticleModified(),
            getMessageTemplateNewArticleModified());
      }
    }
    if (modifiers != null) {
      for (String modifier : modifiers) {
        addMessage(
            analysis, msgElements, modifier,
            getMessageTitleNewArticleModifier(),
            getMessageTemplateNewArticleModifier());
      }
    }
  }

  /**
   * Add a message on user talk page.
   * 
   * @param analysis Page analysis.
   * @param msgElements Message elements.
   * @param user User to inform.
   * @param titleParam Parameter for the title of the new section.
   * @param templateParam Parameter for the template used to inform.
   */
  private void addMessage(
      PageAnalysis analysis, Collection<String> msgElements,
      String user,
      WPCConfigurationString titleParam,
      WPCConfigurationString templateParam) {
    if ((analysis == null) || (user == null)) {
      return;
    }
    String article = analysis.getPage().getTitle();
    WPCConfiguration wpcConfig = analysis.getWPCConfiguration();

    // Prepare elements
    String message = createMessage(article, msgElements, wpcConfig, templateParam);
    if ((message == null) || (message.trim().length() == 0)) {
      return;
    }
    String globalListTemplate = wpcConfig.getString(WPCConfigurationString.MSG_GLOBAL_LIST_TEMPLATE);
    String globalTemplate = wpcConfig.getString(WPCConfigurationString.MSG_GLOBAL_TEMPLATE);
    String globalTitle = wpcConfig.getString(WPCConfigurationString.MSG_GLOBAL_TITLE);
    String title = wpcConfig.getString(titleParam);
    if (title != null) {
      try {
        title = MessageFormat.format(title, article);
      } catch (IllegalArgumentException e) {
        log.warn("Parameter " + titleParam.getAttributeName() + " has an incorrect format");
      }
    }
    Configuration config = Configuration.getConfiguration();
    String signature = config.getString(null, ConfigurationValueString.SIGNATURE);

    // Retrieve user talk page name
    Namespace userTalkNS = wiki.getWikiConfiguration().getNamespace(Namespace.USER_TALK);
    String userTalk = userTalkNS.getTitle() + ":" + user;
    Page userTalkPage = DataManager.getPage(analysis.getWikipedia(), userTalk, null, null, null);

    // Add message
    try {
      if (globalTitle != null) {
        // Check if global title already exists in the talk page
        List<Section> sections = api.retrieveSections(wiki, userTalkPage);
        Section section = null;
        if (sections != null) {
          for (Section tmpSection : sections) {
            if (globalTitle.equals(tmpSection.getLine())) {
              section = tmpSection;
            }
          }
        }

        if (section == null) {
          // Add the title
          StringBuilder fullMessage = new StringBuilder();
          if ((globalTemplate != null) && (globalTemplate.trim().length() > 0)) {
            fullMessage.append("{{");
            fullMessage.append(globalTemplate.trim());
            fullMessage.append("}}\n");
            if ((signature != null) && (signature.trim().length() > 0)) {
              fullMessage.append(signature.trim());
              fullMessage.append("\n\n");
            }
          }
          if ((globalListTemplate != null) && (globalListTemplate.trim().length() > 0)) {
            fullMessage.append("{{");
            fullMessage.append(globalListTemplate.trim());
            fullMessage.append("}}\n");
          }
          if (title != null) {
            fullMessage.append("== ");
            fullMessage.append(title);
            fullMessage.append(" ==\n");
          }
          fullMessage.append(message);
          api.addNewSection(wiki, userTalkPage, globalTitle, fullMessage.toString(), false);
        } else {
          // Add the message in the existing title
          Integer revisionId = userTalkPage.getRevisionId();
          api.retrieveSectionContents(wiki, userTalkPage, section.getIndex());
          if (revisionId.equals(userTalkPage.getRevisionId())) {
            StringBuilder fullMessage = new StringBuilder();
            fullMessage.append(userTalkPage.getContents());
            if (fullMessage.charAt(fullMessage.length() - 1) != '\n') {
              fullMessage.append("\n");
            }
            fullMessage.append(message);
            api.updateSection(wiki, userTalkPage, globalTitle, section.getIndex(), fullMessage.toString(), false);
          } else {
            System.err.println("Page " + userTalk + " has been modified between two requests");
          }
        }
      } else {
        if (title != null) {
          api.addNewSection(wiki, userTalkPage, title, message, false);
        } else {
          // TODO: No global title, no title => Should append the message at the end
          log.warn("Should add " + message + " in " + userTalk);
        }
      }
    } catch (APIException e) {
      //
    }
  }

  /**
   * Create a message that should be added on user talk page.
   * 
   * @param article Article.
   * @param msgElements Message elements.
   * @param wpcConfig Configuration.
   * @param templateParam Parameter for the template used to inform.
   */
  private String createMessage(
      String article, Collection<String> msgElements,
      WPCConfiguration wpcConfig,
      WPCConfigurationString templateParam) {
    String[] templateElements = wpcConfig.getStringArray(templateParam);
    if ((templateElements == null) ||
        (templateElements.length == 0) ||
        (templateElements[0].trim().length() == 0)) {
      return null;
    }
    StringBuilder message = new StringBuilder();
    message.append("{{");
    message.append(templateElements[0].trim());
    if ((templateElements.length > 1) && (templateElements[1].trim().length() > 0)) {
      message.append("|");
      message.append(templateElements[1].trim());
      message.append("=");
      message.append(article);
    }
    if ((templateElements.length > 2) && (templateElements[2].trim().length() > 0)) {
      String wpcUser = wpcConfig.getString(WPCConfigurationString.USER);
      if ((wpcUser != null) && (wpcUser.trim().length() > 0)) {
        message.append("|");
        message.append(templateElements[2].trim());
        message.append("=");
        message.append(wpcUser);
      }
    }
    if (msgElements != null) {
      for (String msgElement : msgElements) {
        message.append("|");
        message.append(msgElement);
      }
    }
    message.append("}}");
    return message.toString();
  }

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
