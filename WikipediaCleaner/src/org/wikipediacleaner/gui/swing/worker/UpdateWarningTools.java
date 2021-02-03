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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.api.data.Section;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Tools for updating warnings.
 */
public abstract class UpdateWarningTools {

  private final static Logger log = LoggerFactory.getLogger(UpdateWarningTools.class);

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

  /** True if contents is already available in pages. */
  private boolean contentsAvailable;

  /** True if purge page cache should be attempted when errors are not found. */
  private boolean usePurge;

  /** True if this is a simulation. */
  private boolean simulation;

  /** Map for errors. */
  private Map<String, List<String>> errorsMap;

  /** List of articles titles supposed to have the error. */
  private Set<String> articles;

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
    this.usePurge = false;
    this.section0 = useSection0();
    this.api = APIFactory.getAPI();
  }

  /**
   * @param available True if contents is already available in pages.
   */
  public void setContentsAvailable(boolean available) {
    this.contentsAvailable = available;
  }

  /**
   * @return True if contents is already available in pages.
   */
  public boolean getContentsAvailable() {
    return contentsAvailable;
  }

  /**
   * @param purge True if purge page cache should be attempted.
   */
  public void setUsePurge(boolean purge) {
    this.usePurge = purge;
  }

  /**
   * @param simulation True if this is a simulation.
   */
  public void setSimulation(boolean simulation) {
    this.simulation = simulation;
  }

  /**
   * Initialize the errors map.
   */
  public void prepareErrorsMap() {
    this.errorsMap = new HashMap<>();
  }

  /**
   * @return Errors map.
   */
  public Map<String, List<String>> getErrorsMap() {
    return errorsMap;
  }

  // ==========================================================================
  // Warning management
  // ==========================================================================

  /**
   * Update warning for a list of pages.
   * 
   * @param pages List of pages.
   * @param creators For each page title, user who has created the page.
   * @param modifiers For each page title, users who have modified the page.
   * @param stats Statistics.
   * @throws APIException Exception thrown by the API.
   */
  public void updateWarning(
      List<Page> pages,
      Map<String, String> creators,
      Map<String, List<String>> modifiers,
      Stats stats) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }

    // Retrieve information in the pages
    if (!retrievePageInformation(pages)) {
      return;
    }

    // Deal with non encyclopedic pages
    manageNonEncyclopedicPages(pages);

    // Load talk pages and "To do" sub pages
    Map<Page, Page> mapTalkPages = new HashMap<>();
    Map<Page, Page> mapTodoSubpages = new HashMap<>();
    for (Page page : pages) {
      Page talkPage = page.getTalkPage();
      mapTalkPages.put(page, talkPage);
      String todoSubpageAttr = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
      if (todoSubpageAttr != null) {
        Page todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        mapTodoSubpages.put(page, todoSubpage);
      }
    }
    if (canUpdateWarning()) {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);
      if (section0) {
        mw.retrieveSectionContents(wiki, mapTalkPages.values(), 0, false);
      } else {
        mw.retrieveContents(wiki, mapTalkPages.values(), false, false, false, false);
      }
      mw.retrieveContents(wiki, mapTodoSubpages.values(), true, false, false, false);
      if (mw.shouldStop()) {
        return;
      }
    }

    // Update warning
    for (Page page : pages) {
      PageAnalysis pageAnalysis = page.getAnalysis(page.getContents(), true);
      boolean updated = updateWarning(
          pageAnalysis, page.getRevisionId(),
          mapTalkPages.get(page),
          mapTodoSubpages.get(page),
          (creators != null) ? creators.get(page.getTitle()) : null,
          (modifiers != null) ? modifiers.get(page.getTitle()) : null,
          stats);
      if (updated) {
        // log.debug("Page " + page.getTitle() + " has been updated.");
      }
      if (stats != null) {
        stats.addAnalyzedPage(page);
        if (updated) {
          stats.addUpdatedPage(page);
        }
      }
    }
    return;
  }

  /**
   * Manage talk pages present in the list.
   * 
   * @param pages List of pages.
   * @throws APIException Exception thrown by the API.
   */
  private void manageNonEncyclopedicPages(List<Page> pages)
      throws APIException {
    if (pages == null) {
      return;
    }
    Iterator<Page> itPage = pages.iterator();
    List<Integer> encyclopedicNamespaces = configuration.getEncyclopedicNamespaces();
    while (itPage.hasNext()) {
      Page page = itPage.next();
      if (!page.isArticle() ||
          !encyclopedicNamespaces.contains(page.getNamespace())) {
        itPage.remove();
        if (!simulation) {
          PageAnalysis analysis = page.getAnalysis(page.getContents(), true);
          Collection<String> elements = constructWarningElements(analysis, null, null);
          if ((elements == null) || (elements.isEmpty())) {
            purgePage(page);
          }
        }
      }
    }
  }

  /**
   * @return true if warnings can be updated.
   */
  public boolean canUpdateWarning() {
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if ((todoTemplates == null) ||
        (todoTemplates.isEmpty())) {
      return false;
    }
    String warningTemplate = configuration.getString(getWarningTemplate());
    if ((warningTemplate == null) || (warningTemplate.trim().length() == 0)) {
      return false;
    }
    return true;
  }

  /**
   * Update warning for a page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the elements for the warning).
   * @param pageRevId Page revision id.
   * @param talkPage (Optional) Talk page with contents of section 0.
   * @param todoSubpage (Optional) To do sub-page with contents.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  public boolean updateWarning(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page talkPage, Page todoSubpage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    if ((pageAnalysis == null) ||
        (pageAnalysis.getPage() == null) ||
        !pageAnalysis.getPage().isArticle()) {
      return false;
    }
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if ((todoTemplates == null) ||
        (todoTemplates.isEmpty())) {
      return false;
    }
    String warningTemplate = configuration.getString(getWarningTemplate());
    if ((warningTemplate == null) || (warningTemplate.trim().length() == 0)) {
      if (!simulation) {
        return false;
      }
    }
    Page page = pageAnalysis.getPage();

    // Retrieving talk page contents
    if (talkPage == null) {
      talkPage = page.getTalkPage();
      setText(GT._T("Retrieving page contents - {0}", talkPage.getTitle()));
      if (section0) {
        api.retrieveSectionContents(wiki, talkPage, 0);
      } else {
        api.retrieveContents(wiki, Collections.singletonList(talkPage), false, false);
      }
    }

    // "To do" sub-page
    String todoSubpageAttr = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
    if (todoSubpageAttr != null) {

      // Retrieving "To do" sub-page contents
      if (todoSubpage == null) {
        todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        setText(GT._T("Retrieving page contents - {0}", todoSubpage.getTitle()));
        api.retrieveContents(wiki, Collections.singletonList(todoSubpage), false, false);
      }

      // If we force the use of "To do" sub-page, the warning must be on it
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.MAIN)) {
        if (configuration.getBoolean(WPCConfigurationBoolean.TODO_SUBPAGE_FORCE)) {
          return manageWarningOnTodoSubpage(
              pageAnalysis, pageRevId, todoSubpage, talkPage,
              creator, modifiers, stats);
        }
      } else if (configuration.getBoolean(WPCConfigurationBoolean.TODO_SUBPAGE_FORCE_OTHER)) {
        return manageWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If "To do" sub-page exists, the warning must be on it
      if (Boolean.TRUE.equals(todoSubpage.isExisting())) {
        return manageWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If talk page has a template linking to the "To do" sub-page,
      // the warning must be on the "To do" sub-page
      PageElementTemplate templateTodoLink = getExistingTemplateTodoLink(talkPage, talkPage.getContents());
      if (templateTodoLink != null) {
        return manageWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If talk page has a link to the "To do" sub-page,
      // the warning must be on the "To do" sub-page
      /*api.retrieveLinks(wikipedia, talkPage, talkPage.getNamespace());
      if (talkPage.getLinks() != null) {
        for (Page link : talkPage.getLinks()) {
          if (Page.areSameTitle(link.getTitle(), todoSubpage.getTitle())) {
            return manageWarningOnTodoSubpage(pageAnalysis, pageRevId, todoSubpage, talkPage);
          }
        }
      }*/
    }

    return manageWarningOnTalkPage(
        pageAnalysis, pageRevId, talkPage,
        creator, modifiers, stats);
  }

  /**
   * Update warning on the "To do" sub-page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the elements for the warning).
   * @param pageRevId Page revision id.
   * @param todoSubpage "To do" sub-page.
   * @param talkPage Talk page.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  private boolean manageWarningOnTodoSubpage(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page todoSubpage, Page talkPage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    Collection<String> elements = constructWarningElements(pageAnalysis, talkPage, todoSubpage);
    boolean result = false;
    if ((elements == null) || (elements.isEmpty())) {
      if (!simulation) {
        boolean resultTodo = removeWarningOnTodoSubpage(todoSubpage);
        boolean resultTalk = removeWarningOnTalkPage(talkPage);
        result = resultTodo || resultTalk;
        if (result) {
          if (!resultTalk) {
            purgePage(talkPage);
          }
          if (stats != null) {
            stats.addRemovedWarning(pageAnalysis.getPage());
          }
        } else {
          purgeArticle(pageAnalysis.getPage());
        }
      }
    } else {
      if (!simulation) {
        result |= updateWarningOnTodoSubpage(
            pageRevId, todoSubpage, elements, creator, modifiers);
        if (createWarning) {
          result |= cleanWarningOnTalkPage(talkPage, elements);
        }
      }
      if (stats != null) {
        stats.addLinks(pageAnalysis.getPage(), elements.size());
      }
    }
    return result;
  }

  /**
   * Update warning on the talk page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the elements for the warning).
   * @param pageRevId Page revision id.
   * @param talkPage Talk page.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  private boolean manageWarningOnTalkPage(
      PageAnalysis pageAnalysis, Integer pageRevId, Page talkPage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    Collection<String> elements = constructWarningElements(pageAnalysis, talkPage, null);
    boolean result = false;
    if ((elements == null) || (elements.isEmpty())) {
      if (!simulation) {
        result = removeWarningOnTalkPage(talkPage);
        if (result) {
          if (stats != null) {
            stats.addRemovedWarning(pageAnalysis.getPage());
          }
        } else {
          purgeArticle(pageAnalysis.getPage());
        }
      }
    } else {
      if (!simulation) {
        result |= updateWarningOnTalkPage(
            pageAnalysis, pageRevId, talkPage, elements, creator, modifiers);
      }
      if (stats != null) {
        stats.addLinks(pageAnalysis.getPage(), elements.size());
      }
    }
    return result;
  }

  /**
   * Create/update warning on the "To do" sub-page.
   * 
   * @param pageRevId Page revision id.
   * @param todoSubpage "To do" sub-page.
   * @param elements Elements for the warning.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  private boolean updateWarningOnTodoSubpage(
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
          getWarningComment(elements),
          !Boolean.TRUE.equals(todoSubpage.isExisting()), false);

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
          getWarningComment(elements),
          false, true, automaticEdit, false);

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
   * @throws APIException Exception thrown by the API.
   */
  private boolean updateWarningOnTalkPage(
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
      PageElementTemplate templateTmp = talkAnalysis.hasTemplate(todoTemplate);
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
      if ((indexStart == 0) && (talkPage.getRedirects().isRedirect())) {
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
      tmp.append("|\n* ");
      addWarning(tmp, pageRevId, elements);
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      String comment = getWarningComment(elements);
      updateTalkPage(talkPage, tmp.toString(), comment, false);

      // Inform creator and modifiers of the page
      informContributors(analysis, elements, creator, modifiers);

      return true;
    }

    // Search warning in the "To do" parameter
    String parameter = templateTodo.getParameterValue("1");
    if (parameter == null) {
      parameter = "";
    }
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
      StringBuilder tmpParameter = new StringBuilder(parameter);
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
      String comment = getWarningComment(elements);
      updateTalkPage(talkPage, tmp.toString(), comment, false);
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
      String comment = getWarningComment(elements);
      updateTalkPage(talkPage, tmp.toString(), comment, false);
      return true;
    }

    return false;
  }

  /**
   * Remove warning on the "To do" sub-page.
   * 
   * @param todoSubpage "To do" sub-page.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  private boolean removeWarningOnTodoSubpage(Page todoSubpage) throws APIException {
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
    String reason = getWarningCommentDone();
    if ((newContents.trim().length() == 0) &&
        (wiki.getConnection().getUser() != null) &&
        (wiki.getConnection().getUser().hasRight(User.RIGHT_DELETE))) {
      api.deletePage(wiki, todoSubpage, reason, automaticEdit);
    } else {
      if (newContents.trim().length() == 0) {
        String delete = configuration.getString(WPCConfigurationString.TODO_SUBPAGE_DELETE);
        if ((delete != null) && (delete.trim().length() > 0)) {
          newContents = delete;
        }
      }
      updatePage(todoSubpage, newContents, reason, true, false);
    }

    return true;
  }

  /**
   * Remove warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @param elements Elements for the warning.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  private boolean cleanWarningOnTalkPage(
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
      String comment = getWarningComment(elements);
      String newContents = TemplateBuilder.from(todoTemplates.get(0)).toString();
      updateTalkPage(talkPage, newContents, comment, false);
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
      PageElementTemplate template = analysis.hasTemplate(templateName);
      if (template != null) {
        templateTodo = template;
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
      tmp.append(TemplateBuilder.from(todoTemplates.get(0)).toString());
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      String comment = getWarningComment(elements);
      updateTalkPage(talkPage, tmp.toString(), comment, false);
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
      String comment = getWarningComment(elements);
      updateTalkPage(talkPage, tmp.toString(), comment, false);
      return true;
    }

    return false;
  }

  /**
   * Remove warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @return True if the warning has been updated.
   * @throws APIException Exception thrown by the API.
   */
  private boolean removeWarningOnTalkPage(
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
        PageElementTemplate template = analysis.hasTemplate(templateName);
        if (template != null) {
          templateTodo = template;
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
        String comment = getWarningCommentDone();
        updateTalkPage(talkPage, tmp.toString(), comment, true);
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
    TemplateBuilder builder = TemplateBuilder.from(" " + configuration.getString(getWarningTemplate()) + " ");
    if (pageRevId != null) {
      builder.addParam(" revisionid", pageRevId.toString() + " ");
    }
    for (String param : params) {
      builder.addParam(" " + param + " ");
    }
    talkText.append(builder.toString());
    talkText.append(" -- ~~~~~");
    String comment = configuration.getString(getWarningTemplateComment());
    if (comment != null) {
      talkText.append(CommentBuilder.from(comment).toString());
    }
  }

  // ==========================================================================
  // Page analysis
  // ==========================================================================

  /**
   * Retrieve information in the pages to construct the warning.
   * 
   * @param pages List of pages.
   * @return True if information was retrieved.
   * @throws APIException Exception thrown by the API.
   */
  protected abstract boolean retrievePageInformation(
      List<Page> pages) throws APIException;

  /**
   * Construct elements for the warning.
   * 
   * @param analysis Page analysis.
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return Warning elements.
   */
  protected abstract Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage);

  /**
   * Memorize an error.
   * 
   * @param error Error to memorize. 
   * @param title Page title in which the error is present.
   */
  protected void memorizeError(String error, String title) {
    if ((errorsMap == null) || (error == null) || (title == null)) {
      return;
    }
    List<String> titles = errorsMap.get(error);
    if (titles == null) {
      titles = new ArrayList<>();
      errorsMap.put(error, titles);
    }
    titles.add(title);
  }

  /**
   * @param talkPage Talk page
   * @param contents Talk page contents.
   * @return Template containing a list to the to do sub-page.
   */
  private PageElementTemplate getExistingTemplateTodoLink(Page talkPage, String contents) {
    PageElementTemplate templateTodoLink = null;
    List<String> todoLinkTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_LINK_TEMPLATES);
    if (todoLinkTemplates != null) {
      PageAnalysis analysis = talkPage.getAnalysis(contents, true);
      for (String todoLink : todoLinkTemplates) {
        PageElementTemplate template = analysis.hasTemplate(todoLink);
        if (template != null) {
          templateTodoLink = template;
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
    if (analysis == null) {
      return null;
    }
    return analysis.hasTemplate(configuration.getString(getWarningTemplate()));
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
            fullMessage.append(TemplateBuilder.from(globalTemplate.trim()).toString());
            fullMessage.append("\n");
            if ((signature != null) && (signature.trim().length() > 0)) {
              fullMessage.append(signature.trim());
              fullMessage.append("\n\n");
            }
          }
          if ((globalListTemplate != null) && (globalListTemplate.trim().length() > 0)) {
            fullMessage.append(TemplateBuilder.from(globalListTemplate.trim()).toString());
            fullMessage.append("\n");
          }
          if (title != null) {
            fullMessage.append("== ");
            fullMessage.append(title);
            fullMessage.append(" ==\n");
          }
          fullMessage.append(message);
          api.addNewSection(
              wiki, userTalkPage, globalTitle,
              fullMessage.toString(),
              true, true, automaticEdit, false);
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
            api.updateSection(
                wiki, userTalkPage, globalTitle, section.getIndex(),
                fullMessage.toString(), true, true, automaticEdit, false);
          } else {
            System.err.println("Page " + userTalk + " has been modified between two requests");
          }
        }
      } else {
        if (title != null) {
          api.addNewSection(
              wiki, userTalkPage, title,
              message, true, true, automaticEdit, false);
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
    TemplateBuilder builder = TemplateBuilder.from(templateElements[0].trim());
    if ((templateElements.length > 1) && (templateElements[1].trim().length() > 0)) {
      builder.addParam(templateElements[1].trim(), article);
    }
    if ((templateElements.length > 2) && (templateElements[2].trim().length() > 0)) {
      String wpcUser = wpcConfig.getString(WPCConfigurationString.USER);
      if ((wpcUser != null) && (wpcUser.trim().length() > 0)) {
        builder.addParam(templateElements[2].trim(), wpcUser);
      }
    }
    if (msgElements != null) {
      for (String msgElement : msgElements) {
        builder.addParam(msgElement);
      }
    }
    return builder.toString();
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
   * @param bot True if the edit should be flagged as bot.
   * @return Result of the command.
   * @throws APIException Exception thrown by the API.
   */
  protected QueryResult updateTalkPage(
      Page page, String newContents,
      String comment, boolean bot) throws APIException {
    if (section0) {
      return updateSection(page, comment, 0, newContents, bot, false);
    }
    return updatePage(page, newContents, comment, bot, false);
  }

  /**
   * Update a page on Wiki.
   * 
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @param bot True if the edit should be flagged as bot.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException Exception thrown by the API.
   */
  protected QueryResult updatePage(
      Page page,
      String newContents, String comment,
      boolean bot, boolean forceWatch) throws APIException {
    return api.updatePage(
        wiki, page, newContents, comment,
        bot, true, automaticEdit, forceWatch);
  }

  /**
   * Update a section in a page.
   * 
   * @param page Page.
   * @param title Title of the new section.
   * @param section Section. 
   * @param contents Contents.
   * @param bot True if the edit should be flagged as bot.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException Exception thrown by the API.
   */
  protected QueryResult updateSection(
      Page page, String title, int section,
      String contents, boolean bot, boolean forceWatch) throws APIException {
    return api.updateSection(
        wiki, page, title, section, contents,
        bot, true, automaticEdit, forceWatch);
  }

  /**
   * Purge a page cache.
   * 
   * @param page Page.
   * @throws APIException Exception thrown by the API.
   */
  protected void purgePage(Page page) throws APIException {
    api.purgePageCache(wiki, page);
  }

  /**
   * @param articles List of articles.
   */
  void setArticles(Set<String> articles) {
    this.articles = new HashSet<>();
    if (articles != null) {
      this.articles.addAll(articles);
    }
  }

  /**
   * @param article Article.
   */
  void addArticle(String article) {
    if (articles == null) {
      articles = new HashSet<>();
    }
    articles.add(article);
  }

  /**
   * Purge an article  when error is not found but article is still listed.
   * 
   * @param page Article to purge.
   * @throws APIException Exception thrown by the API.
   */
  protected void purgeArticle(Page page) throws APIException {
    if (!usePurge) {
      return;
    }
    if ((page == null) || (articles == null)) {
      return;
    }
    if (articles.contains(page.getTitle())) {
      api.purgePageCache(wiki, page);
    }
  }

  /**
   * @return True if the analyze should stop.
   */
  protected boolean shouldStop() {
    if (window == null) {
      return false;
    }
    if ((window.getParentComponent() == null) ||
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

  /**
   * Extract a sub list of pages from a list.
   * 
   * @param list List (extracted pages are removed from the list).
   * @param max Maximum number of pages.
   * @param talkPages True if talk pages should be included.
   * @return Sub list of pages.
   */
  public List<Page> extractSublist(LinkedList<Page> list, int max, boolean talkPages) {
    if (list == null) {
      return null;
    }
    List<Page> sublist = new ArrayList<>(Math.min(max, list.size()));
    while ((sublist.size() < max) && !list.isEmpty()) {
      Page page = list.removeFirst();
      if (talkPages || page.isArticle()) {
        sublist.add(page);
      }
    }
    return sublist;
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
      updatedPages = new ArrayList<>();
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

  /**
   * Display statistics.
   * 
   * @param window Window.
   * @param stats Statistics.
   * @param startTime Start time.
   */
  public static void displayStats(
      BasicWindow window,
      Stats stats, long startTime) {
    if (window == null) {
      return;
    }
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
        window.getParentComponent(), message.toString());
  }
}
