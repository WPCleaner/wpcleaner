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
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Tools for updating warnings.
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

  // ==========================================================================
  // Warning management
  // ==========================================================================

  /**
   * Create/update warning on the "To do" sub-page.
   * 
   * @param pageRevId Page revision id.
   * @param todoSubpage "To do" sub-page.
   * @param elements Elements for the warning.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @return True if the warning has been updated.
   * @throws APIException
   */
  // TODO: private?
  protected boolean updateWarningOnTodoSubpage(
      Integer pageRevId, Page todoSubpage, Collection<String> elements,
      String creator, List<String> modifiers) throws APIException {
    if ((todoSubpage == null) || (elements == null)) {
      return false;
    }

    // Search warning in the "To do" sub-page
    String contents = todoSubpage.getContents();
    if (contents == null) {
      contents = "";
    }
    PageAnalysis analysis = todoSubpage.getAnalysis(contents, true);
    PageElementTemplate templateWarning = getFirstWarningTemplate(analysis);

    // If warning is missing, add it
    if (templateWarning == null) {
      if (!createWarning) {
        return false;
      }
      setText(getMessageUpdateWarning(todoSubpage.getTitle()));
      StringBuilder tmp = new StringBuilder(contents);
      if ((tmp.length() > 0) && (tmp.charAt(tmp.length() - 1) != '\n')) {
        tmp.append('\n');
      }
      tmp.append("* ");
      addWarning(tmp, pageRevId, elements);
      tmp.append('\n');
      updatePage(
          todoSubpage, tmp.toString(),
          wiki.formatComment(
              getWarningComment(elements),
              automaticEdit),
          false);

      // Inform creator and modifiers of the page
      informContributors(analysis, elements, creator, modifiers);

      return true;
    }

    // Check if modifications are needed
    if (isModified(elements, templateWarning)) {
      StringBuilder tmp = new StringBuilder();
      int index = templateWarning.getBeginIndex();
      while ((index > 0) && (contents.charAt(index) != '\n')) {
        index--;
      }
      if (index > 0) {
        tmp.append(contents.substring(0, index));
        tmp.append('\n');
      }
      tmp.append("* ");
      addWarning(tmp, pageRevId, elements);
      tmp.append('\n');
      index = templateWarning.getEndIndex();
      while ((index < contents.length()) && (contents.charAt(index) != '\n')) {
        index++;
      }
      index++;
      if (index < contents.length()) {
        tmp.append(contents.substring(index));
      }
      api.updatePage(
          wiki, todoSubpage, tmp.toString(),
          wiki.formatComment(
              getWarningComment(elements),
              automaticEdit),
          false);

      return true;
    }

    return false;
  }

  /**
   * Create/update warning on the talk page.
   * 
   * @param analysis Page analysis.
   * @param pageRevId Page revision id.
   * @param talkPage Talk page.
   * @param elements Elements for the warning.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @return True if the warning has been updated.
   * @throws APIException
   */
  // TODO: private ?
  protected boolean updateWarningOnTalkPage(
      PageAnalysis analysis, Integer pageRevId,
      Page talkPage, Collection<String> elements,
      String creator, List<String> modifiers) throws APIException {
    if ((talkPage == null) || (elements == null)) {
      return false;
    }

    // Search "To do" template in the talk page
    String contents = talkPage.getContents();
    if (contents == null) {
      contents = "";
    }
    PageAnalysis talkAnalysis = talkPage.getAnalysis(contents, true);
    PageElementTemplate templateTodo = null;
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if ((todoTemplates == null) ||
        (todoTemplates.isEmpty())) {
      return false;
    }
    for (String todoTemplate : todoTemplates) {
      List<PageElementTemplate> templates = talkAnalysis.getTemplates(todoTemplate);
      PageElementTemplate templateTmp = (templates != null) && (templates.size() > 0) ?
          templates.get(0) : null;
      if (templateTmp != null) {
        if ((templateTodo == null) || (templateTmp.getBeginIndex() < templateTodo.getBeginIndex())) {
          templateTodo = templateTmp;
        }
      }
    }

    // If "To do" template is missing, add it
    if (templateTodo == null) {
      if (!createWarning) {
        return false;
      }

      // Search where to add "To do" template
      PageElementTemplate templatePrevious = null;
      List<String> warningAfterTemplates = configuration.getStringList(
          WPCConfigurationStringList.WARNING_AFTER_TEMPLATES);
      if (warningAfterTemplates != null) {
        for (String previousTemplate : warningAfterTemplates) {
          Collection<PageElementTemplate> templates = talkAnalysis.getTemplates(previousTemplate);
          for (PageElementTemplate templateTmp : templates) {
            if ((templatePrevious == null) ||
                (templateTmp.getEndIndex() > templatePrevious.getEndIndex())) {
              templatePrevious = templateTmp;
            }
          }
        }
      }
      int indexStart = (templatePrevious != null) ? templatePrevious.getEndIndex() : 0;
      if ((indexStart == 0) && (talkPage.isRedirect())) {
        indexStart = contents.length();
      }

      // Add warning
      setText(getMessageUpdateWarning(talkPage.getTitle()));
      StringBuilder tmp = new StringBuilder();
      if (indexStart > 0) {
        tmp.append(contents.substring(0, indexStart));
        if (tmp.charAt(tmp.length() - 1) != '\n') {
          tmp.append("\n");
        }
      }
      tmp.append("{{");
      tmp.append(todoTemplates.get(0));
      tmp.append("|* ");
      addWarning(tmp, pageRevId, elements);
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      String comment = wiki.formatComment(
          getWarningComment(elements),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);

      // Inform creator and modifiers of the page
      informContributors(analysis, elements, creator, modifiers);

      return true;
    }

    // Search warning in the "To do" parameter
    String parameter = templateTodo.getParameterValue("1");
    PageAnalysis parameterAnalysis = talkPage.getAnalysis(parameter, false);
    PageElementTemplate templateWarning = getFirstWarningTemplate(parameterAnalysis);
    if (templateWarning == null) {
      StringBuilder tmp = new StringBuilder();
      int indexStart = templateTodo.getBeginIndex();
      if (indexStart > 0) {
        tmp.append(contents.substring(0, indexStart));
        if (tmp.charAt(tmp.length() - 1) != '\n') {
          tmp.append("\n");
        }
      }
      StringBuilder tmpParameter = new StringBuilder((parameter != null) ? parameter : "");
      if ((tmpParameter.length() == 0) ||
          (tmpParameter.charAt(tmpParameter.length() - 1) != '\n')) {
        tmpParameter.append("\n");
      }
      tmpParameter.append("* ");
      addWarning(tmpParameter, pageRevId, elements);
      tmpParameter.append("\n");
      tmp.append(templateTodo.getParameterReplacement("1", tmpParameter.toString(), null));
      int indexEnd = templateTodo.getEndIndex();
      if (indexEnd < contents.length()) {
        if ((tmp.charAt(tmp.length() - 1) != '\n') &&
            (contents.charAt(indexEnd) != '\n')) {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexEnd));
      }
      String comment = wiki.formatComment(
          getWarningComment(elements),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }

    // Update warning if necessary
    if (isModified(elements, templateWarning)) {
      StringBuilder tmp = new StringBuilder();
      tmp.append(contents.substring(0, templateTodo.getBeginIndex()));
      StringBuilder tmpParameter = new StringBuilder();
      if (templateWarning.getBeginIndex() > 0) {
        tmpParameter.append(parameter.substring(0, templateWarning.getBeginIndex()));
      }
      addWarning(tmpParameter, pageRevId, elements);
      int endIndex = parameter.indexOf('\n', templateWarning.getEndIndex());
      if ((endIndex >= 0) && (endIndex < parameter.length())) {
        tmpParameter.append(parameter.substring(endIndex));
      }
      tmp.append(templateTodo.getParameterReplacement("1", tmpParameter.toString(), null));
      if (templateTodo.getEndIndex() < contents.length()) {
        tmp.append(contents.substring(templateTodo.getEndIndex()));
      }
      String comment = wiki.formatComment(
          getWarningComment(elements),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }

    return false;
  }

  /**
   * Remove warning on the "To do" sub-page.
   * 
   * @param todoSubpage "To do" sub-page.
   * @return True if the warning has been updated.
   * @throws APIException
   */
  // TODO: private ?
  protected boolean removeWarningOnTodoSubpage(Page todoSubpage) throws APIException {
    // Check if page is already empty
    if ((todoSubpage == null) || (Boolean.FALSE.equals(todoSubpage.isExisting()))) {
      return false;
    }
    String contents = todoSubpage.getContents();
    if ((contents == null) || (contents.trim().equals(""))) {
      return false;
    }
    PageAnalysis analysis = todoSubpage.getAnalysis(contents, true);

    // Search warning in the "To do" sub-page
    PageElementTemplate template = getFirstWarningTemplate(analysis);
    if (template == null) {
      return false;
    }

    // Analyze text to remove the warning
    setText(getMessageRemoveWarning(todoSubpage.getTitle()));
    StringBuilder tmp = new StringBuilder();
    int index = template.getBeginIndex();
    while ((index > 0) && (contents.charAt(index) != '\n')) {
      index--;
    }
    if (index > 0) {
      tmp.append(contents.substring(0, index));
    }
    index = template.getEndIndex();
    while ((index < contents.length()) && (contents.charAt(index) != '\n')) {
      index++;
    }
    if (index < contents.length()) {
      if (tmp.length() > 0) {
        tmp.append('\n');
      }
      tmp.append(contents.substring(index));
    }

    // Remove the warning
    String newContents = tmp.toString();
    String reason = wiki.formatComment(getWarningCommentDone(), automaticEdit);
    if ((newContents.trim().length() == 0) &&
        (wiki.getConnection().getUser() != null) &&
        (wiki.getConnection().getUser().hasRight(User.RIGHT_DELETE))) {
      api.deletePage(wiki, todoSubpage, reason);
    } else {
      if (newContents.trim().length() == 0) {
        String delete = configuration.getString(WPCConfigurationString.TODO_SUBPAGE_DELETE);
        if ((delete != null) && (delete.trim().length() > 0)) {
          newContents = delete;
        }
      }
      updatePage(todoSubpage, newContents, reason, false);
    }

    return true;
  }

  /**
   * Remove warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @param elements Elements for the warning.
   * @return True if the warning has been updated.
   * @throws APIException
   */
  // TOOD: private?
  protected boolean cleanWarningOnTalkPage(
      Page talkPage, Collection<String> elements) throws APIException {
    // Check if page exists
    if (talkPage == null) {
      return false;
    }
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if ((todoTemplates == null) || (todoTemplates.isEmpty())) {
      return false;
    }
    if (Boolean.FALSE.equals(talkPage.isExisting())) {
      String comment = wiki.formatComment(
          getWarningComment(elements),
          automaticEdit);
      String newContents = "{{" + todoTemplates.get(0) + "}}";
      updateTalkPage(talkPage, newContents, comment);
      return true;
    }

    String contents = talkPage.getContents();
    if (contents == null) {
      return false;
    }
    PageAnalysis analysis = talkPage.getAnalysis(contents, true);

    // Search "To do" in the talk page
    PageElementTemplate templateTodo = null;
    for (String templateName : todoTemplates) {
      List<PageElementTemplate> templates = analysis.getTemplates(templateName);
      if ((templates != null) && (templates.size() > 0)) {
        templateTodo = templates.get(0);
      }
    }

    // If template is missing, verify that a link to the "To do" sub-page exists
    if (templateTodo == null) {

      // If link exists, nothing more to do
      PageElementTemplate templateTodoLink = getExistingTemplateTodoLink(talkPage, contents);
      if (templateTodoLink != null) {
        return false;
      }

      // Search where to add "To do" template
      PageElementTemplate templatePrevious = null;
      List<String> warningAfterTemplates = configuration.getStringList(
          WPCConfigurationStringList.WARNING_AFTER_TEMPLATES);
      if (warningAfterTemplates != null) {
        for (String previousTemplate : warningAfterTemplates) {
          Collection<PageElementTemplate> templates = analysis.getTemplates(previousTemplate);
          for (PageElementTemplate templateTmp : templates) {
            if ((templatePrevious == null) ||
                (templateTmp.getEndIndex() > templatePrevious.getEndIndex())) {
              templatePrevious = templateTmp;
            }
          }
        }
      }

      // Add warning
      setText(getMessageUpdateWarning(talkPage.getTitle()));
      StringBuilder tmp = new StringBuilder();
      int indexStart = (templatePrevious != null) ? templatePrevious.getEndIndex() : 0;
      if (indexStart > 0) {
        tmp.append(contents.substring(0, indexStart));
        if (tmp.charAt(tmp.length() - 1) != '\n') {
          tmp.append("\n");
        }
      }
      tmp.append("{{");
      tmp.append(todoTemplates.get(0));
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      String comment = wiki.formatComment(
          getWarningComment(elements),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }
    if (templateTodo.getParameterValue("1") == null) {
      return false;
    }

    // Search warning in the "To do" parameter
    String parameter = templateTodo.getParameterValue("1");
    PageAnalysis parameterAnalysis = talkPage.getAnalysis(parameter, false);
    PageElementTemplate templateWarning = getFirstWarningTemplate(parameterAnalysis);
    if (templateWarning != null) {
      setText(getMessageRemoveWarning(talkPage.getTitle()));
      StringBuilder tmp = new StringBuilder();
      if (templateTodo.getBeginIndex() > 0) {
        tmp.append(contents.substring(0, templateTodo.getBeginIndex()));
      }
      String tmpParameter = "";
      int index = templateWarning.getBeginIndex();
      while ((index > 0) && (parameter.charAt(index) != '\n')) {
        index--;
      }
      if (index > 0) {
        tmpParameter += parameter.substring(0, index);
      }
      index = templateWarning.getEndIndex();
      while ((index < parameter.length()) && (parameter.charAt(index) != '\n')) {
        index++;
      }
      if (index < parameter.length()) {
        if (tmpParameter.length() > 0) {
          tmpParameter += "\n";
        }
        tmpParameter += parameter.substring(index);
      }
      if (tmpParameter.length() > 0) {
        if ((tmp.length() > 0) && (tmp.charAt(tmp.length() - 1) != '\n')) {
          tmp.append('\n');
        }
        tmp.append(templateTodo.getParameterReplacement("1", tmpParameter, null));
      } else {
        // Search "To do" link
        PageElementTemplate templateTodoLink = null;
        List<String> todoLinkTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_LINK_TEMPLATES);
        if (todoLinkTemplates != null) {
          for (String templateName : todoLinkTemplates) {
            List<PageElementTemplate> tmpTemplates = analysis.getTemplates(templateName);
            if ((tmpTemplates != null) && (tmpTemplates.size() > 0)) {
              templateTodoLink = tmpTemplates.get(0);
            }
          }
        }
        if (templateTodoLink == null) {
          if ((tmp.length() > 0) && (tmp.charAt(tmp.length() - 1) != '\n')) {
            tmp.append('\n');
          }
          tmp.append(templateTodo.getParameterReplacement("1", null, null));
        }
      }
      if (templateTodo.getEndIndex() < contents.length()) {
        if ((tmp.length() > 0) && (tmp.charAt(tmp.length() - 1) != '\n')) {
          if (contents.charAt(templateTodo.getEndIndex()) != '\n') {
            tmp.append('\n');
          }
        }
        tmp.append(contents.substring(templateTodo.getEndIndex()));
      }
      String comment = wiki.formatComment(
          getWarningComment(elements),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }

    return false;
  }

  /**
   * Remove warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @return True if the warning has been updated.
   * @throws APIException
   */
  // TODO: private ?
  protected boolean removeWarningOnTalkPage(
      Page talkPage) throws APIException {
    // Check if page is already empty
    if ((talkPage == null) || (Boolean.FALSE.equals(talkPage.isExisting()))) {
      return false;
    }
    String contents = talkPage.getContents();
    if (contents == null) {
      return false;
    }
    PageAnalysis analysis = talkPage.getAnalysis(contents, true);

    // Search "To do" in the talk page
    PageElementTemplate templateTodo = null;
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if (todoTemplates != null) {
      for (String templateName : todoTemplates) {
        List<PageElementTemplate> templates = analysis.getTemplates(templateName);
        if ((templates != null) && (templates.size() > 0)) {
          templateTodo = templates.get(0);
        }
      }
    }
    if ((templateTodo != null) && (templateTodo.getParameterValue("1") != null)) {

      // Search warning in the "To do" parameter
      int parameterIndex = templateTodo.getParameterIndex("1");
      String parameter = templateTodo.getParameterValue(parameterIndex);
      int parameterStartIndex = templateTodo.getParameterValueStartIndex(parameterIndex);
      PageElementTemplate templateWarning = getFirstWarningTemplate(analysis);
      if (templateWarning != null) {
        setText(getMessageRemoveWarning(talkPage.getTitle()));
        StringBuilder tmp = new StringBuilder();
        if (templateTodo.getBeginIndex() > 0) {
          tmp.append(contents.substring(0, templateTodo.getBeginIndex()));
        }
        String tmpParameter = "";
        int index = templateWarning.getBeginIndex() - parameterStartIndex;
        while ((index > 0) && (parameter.charAt(index) != '\n')) {
          index--;
        }
        if (index > 0) {
          tmpParameter += parameter.substring(0, index);
        }
        index = templateWarning.getEndIndex() - parameterStartIndex;
        while ((index < parameter.length()) && (parameter.charAt(index) != '\n')) {
          index++;
        }
        if (index < parameter.length()) {
          if (tmpParameter.length() > 0) {
            tmpParameter += "\n";
          }
          tmpParameter += parameter.substring(index);
        }
        if (tmpParameter.length() > 0) {
          tmp.append(templateTodo.getParameterReplacement("1", tmpParameter, null));
        } else {
          //
        }
        if (templateTodo.getEndIndex() < contents.length()) {
          tmp.append(contents.substring(templateTodo.getEndIndex()));
        }
        String comment = wiki.formatComment(getWarningCommentDone(), automaticEdit);
        updateTalkPage(talkPage, tmp.toString(), comment);
        return true;
      }
    }

    return false;
  }

  /**
   * Tell if the template should be modified.
   * 
   * @param params List of parameters for the warning template.
   * @param template Template.
   * @return True if the template should be modified.
   */
  private boolean isModified(Collection<String> params, PageElementTemplate template) {
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
  private void addWarning(
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
   * @param talkPage Talk page
   * @param contents Talk page contents.
   * @return Template containing a list to the todo subpage.
   */
  // TODO: private ?
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
   * @param analysis Page analysis.
   * @return First warning template in the page.
   */
  private final PageElementTemplate getFirstWarningTemplate(PageAnalysis analysis) {
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
  private void informContributors(
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
     * Count of warnings that have been removed.
     */
    private int removedWarningsCount;

    /**
     * Count of links.
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
