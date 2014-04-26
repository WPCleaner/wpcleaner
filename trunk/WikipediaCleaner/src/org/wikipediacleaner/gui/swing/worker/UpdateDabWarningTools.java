/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.InternalLinkCount;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating disambiguation warnings.
 */
public class UpdateDabWarningTools extends UpdateWarningTools {

  private final static Log log = LogFactory.getLog(UpdateDabWarningTools.class);

  private final Map<String, Page> dabPages;
  private final Map<String, Page> nonDabPages;

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateDabWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateDabWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateDabWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, createWarning, automaticEdit);
    this.dabPages = new HashMap<String, Page>();
    this.nonDabPages = new HashMap<String, Page>();
  }

  /**
   * Load all disambiguation pages.
   */
  public void preloadDabPages() {
    dabPages.clear();
    nonDabPages.clear();
    try {
      wiki.loadDisambiguationPages(api);
    } catch (APIException e) {
      log.error("Error preloading disambiguation pages", e);
    }
  }

  /**
   * Update disambiguation warning for a list of pages.
   * 
   * @param pages List of pages.
   * @param contentsAvailable True if contents is already available in pages.
   * @param linksAvailable True if links are already available in pages.
   * @param dabInformationAvailable True if disambiguation information is already available in pages.
   * @param creators For each page title, user who has created the page.
   * @param modifiers For each page title, users who have modified the page.
   * @param stats Statistics.
   * @throws APIException
   */
  public void updateDabWarning(
      List<Page> pages, boolean contentsAvailable,
      boolean linksAvailable, boolean dabInformationAvailable,
      Map<String, String> creators,
      Map<String, List<String>> modifiers,
      Stats stats) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);

    // Retrieving links in each page
    if (!linksAvailable) {
      for (Page page : pages) {
        mw.retrieveAllLinks(wiki, page, Namespace.MAIN, null, false, false);
      }
      mw.block(true);
      if (shouldStop()) {
        return;
      }
    }

    // Retrieving disambiguation information in each page
    boolean hasDisambiguationLink = false;
    if (!dabInformationAvailable) {
      if (!wiki.isDisambiguationPagesLoaded()) {
        List<Page> tmpPages = new ArrayList<Page>();
        for (Page page : pages) {
          for (int numLink = 0; numLink < page.getLinks().size(); numLink++) {
            Page link = page.getLinks().get(numLink);
            if (dabPages.containsKey(link.getTitle())) {
              page.getLinks().set(numLink, dabPages.get(link.getTitle()));
              hasDisambiguationLink = true;
            } else if (nonDabPages.containsKey(link.getTitle())) {
              page.getLinks().set(numLink, nonDabPages.get(link.getTitle()));
            } else {
              tmpPages.add(link);
            }
          }
        }
        if (!tmpPages.isEmpty()) {
          mw.retrieveDisambiguationInformation(wiki, tmpPages, null, false, false, true);
        }
        for (Page page : tmpPages) {
          if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
            dabPages.put(page.getTitle(), page);
            hasDisambiguationLink = true;
          } else {
            nonDabPages.put(page.getTitle(), page);
          }
        }
      } else {
        for (Page page : pages) {
          List<Page> links = page.getLinksWithRedirect();
          for (int numLink = 0; numLink < links.size(); numLink++) {
            Page link = links.get(numLink);
            if (Boolean.TRUE.equals(wiki.isDisambiguationPage(link))) {
              link.setDisambiguationPage(Boolean.TRUE);
              hasDisambiguationLink = true;
            } else {
              link.setDisambiguationPage(Boolean.FALSE);
            }
          }
        }
      }
      if (shouldStop()) {
        return;
      }
    }

    // Retrieving page contents
    if (hasDisambiguationLink && !contentsAvailable) {
      List<Page> tmpPages = new ArrayList<Page>();
      for (Page page : pages) {
        boolean toAdd = false;
        for (Page link : page.getLinks()) {
          if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
            toAdd = true;
          }
        }
        if (toAdd) {
          tmpPages.add(page);
        }
      }
      if (!tmpPages.isEmpty()) {
        mw.retrieveContents(wiki, tmpPages, true, false, false, false);
      }
    }

    // Load talk pages and "To do" sub pages
    Map<Page, Page> mapTalkPages = new HashMap<Page, Page>();
    Map<Page, Page> mapTodoSubpages = new HashMap<Page, Page>();
    for (Page page : pages) {
      Page talkPage = page.getTalkPage();
      mapTalkPages.put(page, talkPage);
      String todoSubpageAttr = configuration.getString(WPCConfigurationString.TODO_SUBPAGE);
      if (todoSubpageAttr != null) {
        Page todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        mapTodoSubpages.put(page, todoSubpage);
      }
    }
    if (section0) {
      mw.retrieveSectionContents(wiki, mapTalkPages.values(), 0, false);
    } else {
      mw.retrieveContents(wiki, mapTalkPages.values(), false, false, false, false);
    }
    mw.retrieveContents(wiki, mapTodoSubpages.values(), true, false, false, false);
    if (mw.shouldStop()) {
      return;
    }

    // Update disambiguation warning
    for (Page page : pages) {
      PageAnalysis pageAnalysis = page.getAnalysis(page.getContents(), true);
      boolean updated = updateDabWarning(
          pageAnalysis, page.getRevisionId(),
          mapTalkPages.get(page),
          mapTodoSubpages.get(page),
          (creators != null) ? creators.get(page.getTitle()) : null,
          (modifiers != null) ? modifiers.get(page.getTitle()) : null,
          stats);
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
   * Update disambiguation warning for a page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param talkPage (Optional) Talk page with contents of section 0.
   * @param todoSubpage (Optional) Todo sub page with contents.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  public boolean updateDabWarning(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page talkPage, Page todoSubpage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    if ((pageAnalysis == null) || (pageAnalysis.getPage() == null)) {
      return false;
    }
    List<String> todoTemplates = configuration.getStringList(WPCConfigurationStringList.TODO_TEMPLATES);
    if ((todoTemplates == null) ||
        (todoTemplates.isEmpty())) {
      return false;
    }
    Page page = pageAnalysis.getPage();

    // Retrieving talk page contents
    if (talkPage == null) {
      talkPage = page.getTalkPage();
      setText(GT._("Retrieving page contents - {0}", talkPage.getTitle()));
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
        setText(GT._("Retrieving page contents - {0}", todoSubpage.getTitle()));
        api.retrieveContents(wiki, Collections.singletonList(todoSubpage), false, false);
      }

      // If we force the use of "To do" sub-page, the disambiguation warning must be on it
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.MAIN)) {
        if (configuration.getBoolean(WPCConfigurationBoolean.TODO_SUBPAGE_FORCE)) {
          return manageDabWarningOnTodoSubpage(
              pageAnalysis, pageRevId, todoSubpage, talkPage,
              creator, modifiers, stats);
        }
      } else if (configuration.getBoolean(WPCConfigurationBoolean.TODO_SUBPAGE_FORCE_OTHER)) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If "To do" sub-page exists, the disambiguation warning must be on it
      if (Boolean.TRUE.equals(todoSubpage.isExisting())) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If talk page has a template linking to the "To do" sub-page,
      // the disambiguation warning must be on the "To do" sub-page
      PageElementTemplate templateTodoLink = getExistingTemplateTodoLink(talkPage, talkPage.getContents());
      if (templateTodoLink != null) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage,
            creator, modifiers, stats);
      }

      // If talk page has a link to the "To do" sub-page,
      // the disambiguation warning must be on the "To do" sub-page
      /*api.retrieveLinks(wikipedia, talkPage, talkPage.getNamespace());
      if (talkPage.getLinks() != null) {
        for (Page link : talkPage.getLinks()) {
          if (Page.areSameTitle(link.getTitle(), todoSubpage.getTitle())) {
            return manageDabWarningOnTodoSubpage(pageAnalysis, pageRevId, todoSubpage, talkPage);
          }
        }
      }*/
    }

    return manageDabWarningOnTalkPage(
        pageAnalysis, pageRevId, talkPage,
        creator, modifiers, stats);
  }

  /**
   * Update disambiguation warning on the "To do" sub-page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param todoSubpage "To do" sub-page.
   * @param talkPage Talk page.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTodoSubpage(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page todoSubpage, Page talkPage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    Collection<String> dabLinks = findDabLinks(pageAnalysis, talkPage, todoSubpage);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result |= removeDabWarningOnTodoSubpage(todoSubpage);
      result |= removeDabWarningOnTalkPage(talkPage);
      if (stats != null) {
        stats.addRemovedWarning(pageAnalysis.getPage());
      }
    } else {
      result |= updateDabWarningOnTodoSubpage(
          pageRevId, todoSubpage, dabLinks, creator, modifiers);
      if (createWarning) {
        result |= cleanDabWarningOnTalkPage(talkPage, dabLinks);
      }
      if (stats != null) {
        stats.addLinks(pageAnalysis.getPage(), dabLinks.size());
      }
    }
    return result;
  }

  /**
   * Update disambiguation warning on the talk page.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param talkPage Talk page.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @param stats Statistics.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTalkPage(
      PageAnalysis pageAnalysis, Integer pageRevId, Page talkPage,
      String creator, List<String> modifiers,
      Stats stats) throws APIException {
    Collection<String> dabLinks = findDabLinks(pageAnalysis, talkPage, null);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result = removeDabWarningOnTalkPage(talkPage);
      if (stats != null) {
        stats.addRemovedWarning(pageAnalysis.getPage());
      }
    } else {
      result |= updateDabWarningOnTalkPage(
          pageAnalysis, pageRevId, talkPage, dabLinks, creator, modifiers);
      if (stats != null) {
        stats.addLinks(pageAnalysis.getPage(), dabLinks.size());
      }
    }
    return result;
  }

  /**
   * Create/update disambiguation warning on the "To do" sub-page.
   * 
   * @param pageRevId Page revision id.
   * @param todoSubpage "To do" sub-page.
   * @param dabLinks List of existing disambiguation links.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean updateDabWarningOnTodoSubpage(
      Integer pageRevId, Page todoSubpage, Collection<String> dabLinks,
      String creator, List<String> modifiers) throws APIException {
    if ((todoSubpage == null) || (dabLinks == null)) {
      return false;
    }

    // Search disambiguation warning in the "To do" sub-page
    String contents = todoSubpage.getContents();
    if (contents == null) {
      contents = "";
    }
    PageAnalysis analysis = todoSubpage.getAnalysis(contents, true);
    PageElementTemplate templateWarning = getFirstWarningTemplate(analysis);

    // If disambiguation warning is missing, add it
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
      addWarning(tmp, pageRevId, dabLinks);
      tmp.append('\n');
      updatePage(
          todoSubpage, tmp.toString(),
          wiki.formatComment(
              getWarningComment(dabLinks),
              automaticEdit),
          false);

      // Inform creator and modifiers of the page
      informContributors(analysis, dabLinks, creator, modifiers);

      return true;
    }

    // Check if modifications are needed
    if (isModified(dabLinks, templateWarning)) {
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
      addWarning(tmp, pageRevId, dabLinks);
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
              getWarningComment(dabLinks),
              automaticEdit),
          false);

      return true;
    }

    return false;
  }

  /**
   * Create/update disambiguation warning on the talk page.
   * 
   * @param analysis Page analysis.
   * @param pageRevId Page revision id.
   * @param talkPage Talk page.
   * @param dabLinks List of existing disambiguation links.
   * @param creator User who has created the page.
   * @param modifiers Users who have modified the page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean updateDabWarningOnTalkPage(
      PageAnalysis analysis, Integer pageRevId,
      Page talkPage, Collection<String> dabLinks,
      String creator, List<String> modifiers) throws APIException {
    if ((talkPage == null) || (dabLinks == null)) {
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
      List<String> dabWarningAfterTemplates = configuration.getStringList(
          WPCConfigurationStringList.DAB_WARNING_AFTER_TEMPLATES);
      if (dabWarningAfterTemplates != null) {
        for (String previousTemplate : dabWarningAfterTemplates) {
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
      addWarning(tmp, pageRevId, dabLinks);
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      String comment = wiki.formatComment(
          getWarningComment(dabLinks),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);

      // Inform creator and modifiers of the page
      informContributors(analysis, dabLinks, creator, modifiers);

      return true;
    }

    // Search disambiguation warning in the "To do" parameter
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
      addWarning(tmpParameter, pageRevId, dabLinks);
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
          getWarningComment(dabLinks),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }

    // Update disambiguation warning if necessary
    if (isModified(dabLinks, templateWarning)) {
      StringBuilder tmp = new StringBuilder();
      tmp.append(contents.substring(0, templateTodo.getBeginIndex()));
      StringBuilder tmpParameter = new StringBuilder();
      if (templateWarning.getBeginIndex() > 0) {
        tmpParameter.append(parameter.substring(0, templateWarning.getBeginIndex()));
      }
      addWarning(tmpParameter, pageRevId, dabLinks);
      int endIndex = parameter.indexOf('\n', templateWarning.getEndIndex());
      if ((endIndex >= 0) && (endIndex < parameter.length())) {
        tmpParameter.append(parameter.substring(endIndex));
      }
      tmp.append(templateTodo.getParameterReplacement("1", tmpParameter.toString(), null));
      if (templateTodo.getEndIndex() < contents.length()) {
        tmp.append(contents.substring(templateTodo.getEndIndex()));
      }
      String comment = wiki.formatComment(
          getWarningComment(dabLinks),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }

    return false;
  }

  /**
   * Remove disambiguation warning on the "To do" sub-page.
   * 
   * @param todoSubpage "To do" sub-page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean removeDabWarningOnTodoSubpage(Page todoSubpage) throws APIException {
    // Check if page is already empty
    if ((todoSubpage == null) || (Boolean.FALSE.equals(todoSubpage.isExisting()))) {
      return false;
    }
    String contents = todoSubpage.getContents();
    if ((contents == null) || (contents.trim().equals(""))) {
      return false;
    }
    PageAnalysis analysis = todoSubpage.getAnalysis(contents, true);

    // Search disambiguation warning in the "To do" sub-page
    PageElementTemplate template = getFirstWarningTemplate(analysis);
    if (template == null) {
      return false;
    }

    // Analyze text to remove the disambiguation warning
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

    // Remove the disambiguation warning
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
   * Remove disambiguation warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @param dabLinks Links to disambiguation pages.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean cleanDabWarningOnTalkPage(
      Page talkPage, Collection<String> dabLinks) throws APIException {
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
          getWarningComment(dabLinks),
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
      List<String> dabWarningAfterTemplates = configuration.getStringList(
          WPCConfigurationStringList.DAB_WARNING_AFTER_TEMPLATES);
      if (dabWarningAfterTemplates != null) {
        for (String previousTemplate : dabWarningAfterTemplates) {
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
          getWarningComment(dabLinks),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }
    if (templateTodo.getParameterValue("1") == null) {
      return false;
    }

    // Search disambiguation warning in the "To do" parameter
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
          getWarningComment(dabLinks),
          automaticEdit);
      updateTalkPage(talkPage, tmp.toString(), comment);
      return true;
    }

    return false;
  }

  /**
   * Remove disambiguation warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean removeDabWarningOnTalkPage(
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
      // Search disambiguation warning in the "To do" parameter
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
   * Extract links to disambiguation pages.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @return List of links to disambiguation pages.
   */
  private Collection<String> findDabLinks(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }
    List<String> dabLinks = new ArrayList<String>();
    List<Page> links = analysis.getPage().getLinks();
    if (links != null) {

      // List disambiguation links in the page
      analysis.countLinks(links);
      for (Page link : links) {
        if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
          InternalLinkCount linkCount = analysis.getLinkCount(link);
          if (linkCount != null) {
            if ((linkCount.getInternalLinkCount() > 0) ||
                (linkCount.getIncorrectTemplateCount() > 0) ||
                (linkCount.getHelpNeededCount() > 0)) {
              dabLinks.add(link.getTitle());
            }
          }
        }
      }

      // Remove links marked as normal
      cleanDabList(dabLinks, talkPage);
      cleanDabList(dabLinks, todoSubpage);
    }
    Collections.sort(dabLinks);
    return dabLinks;
  }

  /**
   * Remove disambiguation links marked as normal from the list.
   * 
   * @param dabList List of disambiguation links.
   * @param page Page containing the list of normal disambiguation links.
   */
  private void cleanDabList(Collection<String> dabList, Page page) {
    if ((dabList == null) || (page == null)) {
      return;
    }
    String okTemplate = configuration.getString(WPCConfigurationString.DAB_OK_TEMPLATE);
    if ((okTemplate == null) || okTemplate.trim().isEmpty()) {
      return;
    }
    okTemplate = okTemplate.trim();
    PageAnalysis analysis = page.getAnalysis(page.getContents(), false);
    List<PageElementTemplate> templates = analysis.getTemplates(okTemplate);
    if ((templates == null) || templates.isEmpty()) {
      return;
    }
    for (PageElementTemplate template : templates) {
      boolean done = false;
      int numParam = 1;
      while (!done) {
        String link = template.getParameterValue(Integer.toString(numParam));
        if (link == null) {
          done = true;
        } else {
          Iterator<String> itDab = dabList.iterator();
          while (itDab.hasNext()) {
            String dab = itDab.next();
            if (Page.areSameTitle(dab, link)) {
              itDab.remove();
            }
          }
          numParam++;
        }
      }
    }
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  @Override
  protected WPCConfigurationString getWarningTemplate() {
    return WPCConfigurationString.DAB_WARNING_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the warning template comment.
   */
  @Override
  protected WPCConfigurationString getWarningTemplateComment() {
    return WPCConfigurationString.DAB_WARNING_TEMPLATE_COMMENT;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTitleNewArticle() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_WITH_DAB_TITLE;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTitleNewArticleModified() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIED_WITH_DAB_TITLE;
  }

  /**
   * @return Configuration parameter for the title for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTitleNewArticleModifier() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIER_WITH_DAB_TITLE;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTemplateNewArticle() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_WITH_DAB_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTemplateNewArticleModified() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIED_WITH_DAB_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the template for a message for a new article.
   */
  @Override
  protected WPCConfigurationString getMessageTemplateNewArticleModifier() {
    return WPCConfigurationString.MSG_NEW_ARTICLE_MODIFIER_WITH_DAB_TEMPLATE;
  }

  /**
   * @return True if section 0 of the talk page should be used.
   */
  @Override
  protected boolean useSection0() {
    return configuration.getBoolean(WPCConfigurationBoolean.DAB_WARNING_SECTION_0);
  }

  /**
   * @return Comment when warning is removed.
   */
  @Override
  protected String getWarningCommentDone() {
    return configuration.getDisambiguationWarningCommentDone();
  }

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  @Override
  protected String getWarningComment(Collection<String> elements) {
    return configuration.getDisambiguationWarningComment(elements);
  }

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  @Override
  protected String getMessageRemoveWarning(String title) {
    return GT._("Removing disambiguation warning - {0}", title);
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._("Updating disambiguation warning - {0}", title);
  }
}
