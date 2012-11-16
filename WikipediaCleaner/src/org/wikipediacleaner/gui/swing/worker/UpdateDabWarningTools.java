/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationAttributeBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationAttributeString;
import org.wikipediacleaner.api.data.InternalLinkCount;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating disambiguation warnings.
 */
public class UpdateDabWarningTools {

  private final EnumWikipedia wikipedia;
  private final WPCConfiguration configuration;
  private final BasicWorker worker;
  private final BasicWindow window;
  private final boolean createWarning;
  private final Map<String, Page> dabPages;
  private final Map<String, Page> nonDabPages;
  private final API api;

  /**
   * @param wikipedia Wikipedia.
   * @param worker Worker.
   */
  public UpdateDabWarningTools(EnumWikipedia wikipedia, BasicWorker worker) {
    this(wikipedia, worker, true);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   */
  public UpdateDabWarningTools(EnumWikipedia wikipedia, BasicWorker worker, boolean createWarning) {
    this(wikipedia, worker, (worker != null) ? worker.getWindow() : null, createWarning);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   */
  public UpdateDabWarningTools(EnumWikipedia wikipedia, BasicWindow window) {
    this(wikipedia, null, window, true);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  private UpdateDabWarningTools(
      EnumWikipedia wikipedia,
      BasicWorker worker, BasicWindow window,
      boolean createWarning) {
    this.wikipedia = wikipedia;
    this.configuration = wikipedia.getConfiguration();
    this.worker = worker;
    this.window = window;
    this.createWarning = createWarning;
    this.dabPages = new HashMap<String, Page>();
    this.nonDabPages = new HashMap<String, Page>();
    this.api = APIFactory.getAPI();
  }

  /**
   * Load all disambiguation pages.
   */
  public void preloadDabPages() {
    dabPages.clear();
    nonDabPages.clear();
    try {
      wikipedia.loadDisambiguationPages(api);
    } catch (APIException e) {
      //
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
   * @return Number of pages updated
   * @throws APIException
   */
  public int updateDabWarning(
      List<Page> pages, boolean contentsAvailable,
      boolean linksAvailable, boolean dabInformationAvailable,
      Map<String, String> creators,
      Map<String, List<String>> modifiers) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return 0;
    }
    MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);

    // Retrieving links in each page
    if (!linksAvailable) {
      for (Page page : pages) {
        mw.retrieveAllLinks(wikipedia, page, Namespace.MAIN, null, false);
      }
      mw.block(true);
      if (shouldStop()) {
        return 0;
      }
    }

    // Retrieving disambiguation information in each page
    boolean hasDisambiguationLink = false;
    if (!dabInformationAvailable) {
      if (!wikipedia.isDisambiguationPagesLoaded()) {
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
          mw.retrieveDisambiguationInformation(wikipedia, tmpPages, null, false, true);
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
            if (Boolean.TRUE.equals(wikipedia.isDisambiguationPage(link))) {
              link.setDisambiguationPage(Boolean.TRUE);
              hasDisambiguationLink = true;
            } else {
              link.setDisambiguationPage(Boolean.FALSE);
            }
          }
        }
      }
      if (shouldStop()) {
        return 0;
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
        mw.retrieveContents(wikipedia, tmpPages, true, false);
      }
    }

    // Load talk pages and Todo sub pages
    Map<Page, Page> mapTalkPages = new HashMap<Page, Page>();
    Map<Page, Page> mapTodoSubpages = new HashMap<Page, Page>();
    for (Page page : pages) {
      Page talkPage = page.getTalkPage();
      mapTalkPages.put(page, talkPage);
      String todoSubpageAttr = configuration.getStringProperty(WPCConfigurationAttributeString.TODO_SUBPAGE);
      if (todoSubpageAttr != null) {
        Page todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        mapTodoSubpages.put(page, todoSubpage);
      }
    }
    mw.retrieveSectionContents(wikipedia, mapTalkPages.values(), 0, false);
    mw.retrieveContents(wikipedia, mapTodoSubpages.values(), true, false);
    if (mw.shouldStop()) {
      return 0;
    }

    // Update disambiguation warning
    int count = 0;
    for (Page page : pages) {
      PageAnalysis pageAnalysis = page.getAnalysis(page.getContents(), true);
      if (updateDabWarning(
          pageAnalysis, page.getRevisionId(),
          mapTalkPages.get(page),
          mapTodoSubpages.get(page),
          (creators != null) ? creators.get(page.getTitle()) : null,
          (modifiers != null) ? modifiers.get(page.getTitle()) : null)) {
        count++;
      }
    }
    return count;
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
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  public boolean updateDabWarning(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page talkPage, Page todoSubpage,
      String creator, List<String> modifiers) throws APIException {
    if ((pageAnalysis == null) || (pageAnalysis.getPage() == null)) {
      return false;
    }
    if ((configuration.getTodoTemplates() == null) ||
        (configuration.getTodoTemplates().isEmpty())) {
      return false;
    }
    Page page = pageAnalysis.getPage();

    // Retrieving talk page contents
    if (talkPage == null) {
      talkPage = page.getTalkPage();
      setText(GT._("Retrieving page contents - {0}", talkPage.getTitle()));
      api.retrieveSectionContents(wikipedia, talkPage, 0);
    }

    // "To do" sub-page
    String todoSubpageAttr = configuration.getStringProperty(WPCConfigurationAttributeString.TODO_SUBPAGE);
    if (todoSubpageAttr != null) {

      // Retrieving "To do" sub-page contents
      if (todoSubpage == null) {
        todoSubpage = talkPage.getSubPage(todoSubpageAttr);
        setText(GT._("Retrieving page contents - {0}", todoSubpage.getTitle()));
        api.retrieveContents(wikipedia, Collections.singletonList(todoSubpage), false);
      }

      // If we force the use of "To do" sub-page, the disambiguation warning must be on it
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.MAIN)) {
        if (configuration.getBooleanProperty(WPCConfigurationAttributeBoolean.TODO_SUBPAGE_FORCE)) {
          return manageDabWarningOnTodoSubpage(
              pageAnalysis, pageRevId, todoSubpage, talkPage, creator, modifiers);
        }
      } else if (configuration.getBooleanProperty(WPCConfigurationAttributeBoolean.TODO_SUBPAGE_FORCE_OTHER)) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage, creator, modifiers);
      }

      // If "To do" sub-page exists, the disambiguation warning must be on it
      if (Boolean.TRUE.equals(todoSubpage.isExisting())) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage, creator, modifiers);
      }

      // If talk page has a template linking to the "To do" sub-page,
      // the disambiguation warning must be on the "To do" sub-page
      PageElementTemplate templateTodoLink = getExistingTemplateTodoLink(talkPage, talkPage.getContents());
      if (templateTodoLink != null) {
        return manageDabWarningOnTodoSubpage(
            pageAnalysis, pageRevId, todoSubpage, talkPage, creator, modifiers);
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
        creator, modifiers);
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
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTodoSubpage(
      PageAnalysis pageAnalysis, Integer pageRevId,
      Page todoSubpage, Page talkPage,
      String creator, List<String> modifiers) throws APIException {
    Collection<String> dabLinks = findDabLinks(pageAnalysis);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result |= removeDabWarningOnTodoSubpage(todoSubpage);
      result |= removeDabWarningOnTalkPage(talkPage);
    } else {
      result |= updateDabWarningOnTodoSubpage(
          pageRevId, todoSubpage, dabLinks, creator, modifiers);
      if (createWarning) {
        result |= cleanDabWarningOnTalkPage(talkPage, dabLinks);
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
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTalkPage(
      PageAnalysis pageAnalysis, Integer pageRevId, Page talkPage,
      String creator, List<String> modifiers) throws APIException {
    Collection<String> dabLinks = findDabLinks(pageAnalysis);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result = removeDabWarningOnTalkPage(talkPage);
    } else {
      result |= updateDabWarningOnTalkPage(
          pageAnalysis, pageRevId, talkPage, dabLinks, creator, modifiers);
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
    List<PageElementTemplate> templates = analysis.getTemplates(
        configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE));
    PageElementTemplate templateWarning = ((templates != null) && (templates.size() > 0)) ?
        templates.get(0) : null;

    // If disambiguation warning is missing, add it
    if (templateWarning == null) {
      if (!createWarning) {
        return false;
      }
      setText(GT._("Updating disambiguation warning - {0}", todoSubpage.getTitle()));
      StringBuilder tmp = new StringBuilder(contents);
      if ((tmp.length() > 0) && (tmp.charAt(tmp.length() - 1) != '\n')) {
        tmp.append('\n');
      }
      tmp.append("* ");
      addWarning(tmp, pageRevId, dabLinks);
      tmp.append('\n');
      updatePage(
          todoSubpage, tmp.toString(),
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          false);
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
          wikipedia, todoSubpage, tmp.toString(),
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          false);

      // Inform creator and modifiers of the page
      informContributors(analysis, creator, modifiers);

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
    if ((configuration.getTodoTemplates() == null) ||
        (configuration.getTodoTemplates().isEmpty())) {
      return false;
    }
    for (String todoTemplate : configuration.getTodoTemplates()) {
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
      if (configuration.getDisambiguationWarningAfterTemplates() != null) {
        for (String previousTemplate : configuration.getDisambiguationWarningAfterTemplates()) {
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
      setText(GT._("Updating disambiguation warning - {0}", talkPage.getTitle()));
      StringBuilder tmp = new StringBuilder();
      if (indexStart > 0) {
        tmp.append(contents.substring(0, indexStart));
        if (tmp.charAt(tmp.length() - 1) != '\n') {
          tmp.append("\n");
        }
      }
      tmp.append("{{");
      tmp.append(configuration.getTodoTemplates().get(0));
      tmp.append("|* ");
      addWarning(tmp, pageRevId, dabLinks);
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      updateSection(
          talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);

      // Inform creator and modifiers of the page
      informContributors(analysis, creator, modifiers);

      return true;
    }

    // Search disambiguation warning in the "To do" parameter
    String parameter = templateTodo.getParameterValue("1");
    PageAnalysis parameterAnalysis = talkPage.getAnalysis(parameter, false);
    List<PageElementTemplate> templates = parameterAnalysis.getTemplates(
        configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE));
    PageElementTemplate templateWarning = (templates != null) && (templates.size() > 0) ?
        templates.get(0) : null;
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
      updateSection(
          talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
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
      updateSection(
          talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
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
    if ((contents == null) || (contents.equals(""))) {
      return false;
    }
    PageAnalysis analysis = todoSubpage.getAnalysis(contents, true);

    // Search disambiguation warning in the "To do" sub-page
    List<PageElementTemplate> templates = analysis.getTemplates(
        configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE));
    PageElementTemplate template = (templates != null) && (templates.size() > 0) ?
        templates.get(0) : null;
    if (template == null) {
      return false;
    }

    // Analyze text to remove the disambiguation warning
    setText(GT._("Removing disambiguation warning - {0}", todoSubpage.getTitle()));
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
    updatePage(
        todoSubpage, tmp.toString(),
        wikipedia.formatComment(configuration.getDisambiguationWarningCommentDone()),
        false);

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
    if (Boolean.FALSE.equals(talkPage.isExisting())) {
      updateSection(
          talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, "{{" + configuration.getTodoTemplates().get(0) + "}}", false);
      return true;
    }

    String contents = talkPage.getContents();
    if (contents == null) {
      return false;
    }
    PageAnalysis analysis = talkPage.getAnalysis(contents, true);

    // Search "To do" in the talk page
    PageElementTemplate templateTodo = null;
    if (configuration.getTodoTemplates() != null) {
      for (String templateName : configuration.getTodoTemplates()) {
        List<PageElementTemplate> templates = analysis.getTemplates(templateName);
        if ((templates != null) && (templates.size() > 0)) {
          templateTodo = templates.get(0);
        }
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
      if (configuration.getDisambiguationWarningAfterTemplates() != null) {
        for (String previousTemplate : configuration.getDisambiguationWarningAfterTemplates()) {
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
      setText(GT._("Updating disambiguation warning - {0}", talkPage.getTitle()));
      StringBuilder tmp = new StringBuilder();
      int indexStart = (templatePrevious != null) ? templatePrevious.getEndIndex() : 0;
      if (indexStart > 0) {
        tmp.append(contents.substring(0, indexStart));
        if (tmp.charAt(tmp.length() - 1) != '\n') {
          tmp.append("\n");
        }
      }
      tmp.append("{{");
      tmp.append(configuration.getTodoTemplates().get(0));
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      updateSection(
          talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
      return true;
    }
    if (templateTodo.getParameterValue("1") == null) {
      return false;
    }

    // Search disambiguation warning in the "To do" parameter
    String parameter = templateTodo.getParameterValue("1");
    PageAnalysis parameterAnalysis = talkPage.getAnalysis(parameter, false);
    List<PageElementTemplate> templates = parameterAnalysis.getTemplates(
        configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE));
    PageElementTemplate templateWarning = (templates != null) && (templates.size() > 0) ?
        templates.get(0) : null;
    if (templateWarning != null) {
      setText(GT._("Removing disambiguation warning - {0}", talkPage.getTitle()));
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
        if (configuration.getTodoLinkTemplates() != null) {
          for (String templateName : configuration.getTodoLinkTemplates()) {
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
      updateSection(
          talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
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
    if (configuration.getTodoTemplates() != null) {
      for (String templateName : configuration.getTodoTemplates()) {
        List<PageElementTemplate> templates = analysis.getTemplates(templateName);
        if ((templates != null) && (templates.size() > 0)) {
          templateTodo = templates.get(0);
        }
      }
    }
    if ((templateTodo != null) && (templateTodo.getParameterValue("1") != null)) {
      // Search disambiguation warning in the "To do" parameter
      String parameter = templateTodo.getParameterValue("1");
      List<PageElementTemplate> templates = analysis.getTemplates(
          configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE));
      PageElementTemplate templateWarning = (templates != null) && (templates.size() > 0) ?
          templates.get(0) : null;
      if (templateWarning != null) {
        setText(GT._("Removing disambiguation warning - {0}", talkPage.getTitle()));
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
          tmp.append(templateTodo.getParameterReplacement("1", tmpParameter, null));
        } else {
          //
        }
        if (templateTodo.getEndIndex() < contents.length()) {
          tmp.append(contents.substring(templateTodo.getEndIndex()));
        }
        updateSection(
            talkPage,
            wikipedia.formatComment(configuration.getDisambiguationWarningCommentDone()),
            0, tmp.toString(), false);
        return true;
      }
    }

    return false;
  }

  /**
   * Inform page contributors of links to disambiguation pages.
   * 
   * @param analysis Page analysis.
   * @param creator User who has created the page.
   */
  private void informContributors(
      PageAnalysis analysis,
      String creator,
      List<String> modifiers) {
    if (analysis == null) {
      return;
    }
    if (creator != null) {
      if ((modifiers == null) || (modifiers.isEmpty())) {
        // TODO
        System.err.println("Should inform " + creator + " for creating page " + analysis.getPage().getTitle());
      } else {
        // TODO
        System.err.println("Should inform " + creator + " for creating page " + analysis.getPage().getTitle() + " (+)");
      }
    }
    if (modifiers != null) {
      for (String modifier : modifiers) {
        // TODO
        System.err.println("Should inform " + modifier + " for modifying page " + analysis.getPage().getTitle());
      }
    }
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
  private QueryResult updatePage(
      Page page,
      String newContents, String comment,
      boolean forceWatch) throws APIException {
    int attemptNumber = 0;
    for (;;) {
      try {
        attemptNumber++;
        return api.updatePage(wikipedia, page, newContents, comment, forceWatch);
      } catch (APIException e) {
        if ((e.getQueryResult() != EnumQueryResult.BAD_TOKEN) || (attemptNumber > 1)) {
          throw e;
        }
      }
    }
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
  private QueryResult updateSection(
      Page page, String title, int section,
      String contents, boolean forceWatch) throws APIException {
    int attemptNumber = 0;
    for (;;) {
      try {
        attemptNumber++;
        return api.updateSection(wikipedia, page, title, section, contents, forceWatch);
      } catch (APIException e) {
        if ((e.getQueryResult() != EnumQueryResult.BAD_TOKEN) || (attemptNumber > 1)) {
          throw e;
        }
      }
    }
  }

  /**
   * Extract links to disambiguation pages.
   * 
   * @param pageAnalysis Page analysis (must have enough information to compute the list of disambiguation links).
   * @return List of links to disambiguation pages.
   */
  private Collection<String> findDabLinks(PageAnalysis pageAnalysis) {
    if ((pageAnalysis == null) || (pageAnalysis.getPage() == null)) {
      return null;
    }
    List<String> dabLinks = new ArrayList<String>();
    List<Page> links = pageAnalysis.getPage().getLinks();
    if (links != null) {
      pageAnalysis.countLinks(links);
      for (Page link : links) {
        if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
          InternalLinkCount linkCount = pageAnalysis.getLinkCount(link);
          if (linkCount != null) {
            if ((linkCount.getInternalLinkCount() > 0) ||
                (linkCount.getIncorrectTemplateCount() > 0) ||
                (linkCount.getHelpNeededTemplateCount() > 0)) {
              dabLinks.add(link.getTitle());
            }
          }
        }
      }
    }
    Collections.sort(dabLinks);
    return dabLinks;
  }

  /**
   * @param talkPage Talk page
   * @param contents Talk page contents.
   * @return Template containing a list to the todo subpage.
   */
  private PageElementTemplate getExistingTemplateTodoLink(Page talkPage, String contents) {
    PageElementTemplate templateTodoLink = null;
    if (configuration.getTodoLinkTemplates() != null) {
      PageAnalysis analysis = talkPage.getAnalysis(contents, true);
      for (String todoLink : configuration.getTodoLinkTemplates()) {
        List<PageElementTemplate> templates = analysis.getTemplates(todoLink);
        if ((templates != null) && (templates.size() > 0)) {
          templateTodoLink = templates.get(0);
        }
      }
    }
    return templateTodoLink;
  }
  /**
   * @param dabLinks Links to disambiguation pages.
   * @return Comment.
   */
  private String getDisambiguationWarningComment(Collection<String> dabLinks) {
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      return configuration.getDisambiguationWarningComment(0);
    }
    StringBuilder result = new StringBuilder(
        configuration.getDisambiguationWarningComment(dabLinks.size()));
    boolean first = true;
    for (String dabLink : dabLinks) {
      if (first) {
        result.append(" - ");
        first = false;
      } else {
        result.append(", ");
      }
      result.append("[[");
      result.append(dabLink);
      result.append("]]");
    }
    return result.toString();
  }

  /**
   * Tell if the template should be modified.
   * 
   * @param dabLinks Links to disambiguation pages.
   * @param template Template.
   * @return True if the template should be modified.
   */
  private boolean isModified(Collection<String> dabLinks, PageElementTemplate template) {
    // Check that links in template are still useful
    int paramNum = 1;
    while (template.getParameterValue(Integer.toString(paramNum)) != null) {
      String link = template.getParameterValue(Integer.toString(paramNum)).trim();
      if (!dabLinks.contains(link)) {
        return true;
      }
      paramNum++;
    }

    // Check that current links are already in the template
    for (String link : dabLinks) {
      boolean found = false;
      paramNum = 1;
      while ((found == false) && (template.getParameterValue(Integer.toString(paramNum)) != null)) {
        if (link.equals(template.getParameterValue(Integer.toString(paramNum)))) {
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
   * Add a disambiguation warning in a text.
   * 
   * @param talkText Text in which the warning should be added.
   * @param pageRevId Page revision id.
   * @param dabLinks List of disambiguation links.
   */
  private void addWarning(
      StringBuilder talkText,
      Integer pageRevId, Collection<String> dabLinks) {
    talkText.append("{{ ");
    talkText.append(configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE));
    if (pageRevId != null) {
      talkText.append(" | revisionid=");
      talkText.append(pageRevId);
    }
    for (String dabLink : dabLinks) {
      talkText.append(" | ");
      talkText.append(dabLink);
    }
    talkText.append(" }} -- ~~~~~");
    String comment = configuration.getStringProperty(WPCConfigurationAttributeString.DAB_WARNING_TEMPLATE_COMMENT);
    if (comment != null) {
      talkText.append(" <!-- ");
      talkText.append(comment);
      talkText.append(" -->");
    }
  }

  /**
   * @return True if the analyze should stop.
   */
  private boolean shouldStop() {
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
  private void setText(String text) {
    if (worker != null) {
      worker.setText(text);
    }
  }
}
