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

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating disambiguation warnings.
 */
public class UpdateDabWarningTools {

  private final EnumWikipedia wikipedia;
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
    this.worker = worker;
    this.window = window;
    this.createWarning = createWarning;
    this.dabPages = new HashMap<String, Page>();
    this.nonDabPages = new HashMap<String, Page>();
    this.api = APIFactory.getAPI();
  }

  /**
   * Update disambiguation warning for a list of pages.
   * 
   * @param pages List of pages.
   * @return Number of pages updated
   * @throws APIException
   */
  public int updateDabWarning(List<Page> pages) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return 0;
    }
    MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);

    // Retrieving links in each page
    for (Page page : pages) {
      mw.retrieveAllLinks(wikipedia, page, Namespace.MAIN, null, false);
    }
    mw.block(true);
    if (shouldStop()) {
      return 0;
    }

    // Retrieving disambiguation information in each page
    List<Page> tmpPages = new ArrayList<Page>();
    boolean hasDisambiguationLink = false;
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
    if (shouldStop()) {
      return 0;
    }

    // Retrieving page contents
    if (hasDisambiguationLink) {
      mw.retrieveContents(wikipedia, pages, true, false);
    }

    // Update disambiguation warning
    int count = 0;
    for (Page page : pages) {
      if (updateDabWarning(page, page.getRevisionId(), page.getContents())) {
        count++;
      }
    }
    return count;
  }

  /**
   * Update disambiguation warning for a page.
   * 
   * @param page Page (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param text Page contents (can be different than the current page text).
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  public boolean updateDabWarning(
      Page page, Integer pageRevId, String text) throws APIException {
    if (page == null) {
      return false;
    }
    if ((wikipedia.getTodoTemplates() == null) ||
        (wikipedia.getTodoTemplates().isEmpty())) {
      return false;
    }

    // Retrieving talk page contents
    Page talkPage = page.getTalkPage(wikipedia.getNamespaces());
    setText(GT._("Retrieving page contents - {0}", talkPage.getTitle()));
    api.retrieveSectionContents(wikipedia, talkPage, 0);

    // Todo subpage
    if (wikipedia.getTodoSubpage() != null) {

      // Retrieving todo subpage contents
      Page todoSubpage = talkPage.getSubPage(wikipedia.getTodoSubpage());
      setText(GT._("Retrieving page contents - {0}", todoSubpage.getTitle()));
      api.retrieveContents(wikipedia, todoSubpage, false);

      // If we force the use of todo subpage, the disambiguation warning must be on it
      if ((page.getNamespace() != null) &&
          (page.getNamespace().intValue() == Namespace.MAIN)) {
        if (wikipedia.getTodoSubpageForce()) {
          return manageDabWarningOnTodoSubpage(page, pageRevId, text, todoSubpage, talkPage);
        }
      } else if (wikipedia.getTodoSubpageForceOther()) {
        return manageDabWarningOnTodoSubpage(page, pageRevId, text, todoSubpage, talkPage);
      }

      // If todo subpage exists, the disambiguation warning must be on it
      if (Boolean.TRUE.equals(todoSubpage.isExisting())) {
        return manageDabWarningOnTodoSubpage(page, pageRevId, text, todoSubpage, talkPage);
      }

      // If talk page has a link to the todo subpage, the disambiguation warning must be on the todo subpage
      api.retrieveLinks(wikipedia, talkPage, talkPage.getNamespace());
      if (talkPage.getLinks() != null) {
        for (Page link : talkPage.getLinks()) {
          if (Page.areSameTitle(link.getTitle(), todoSubpage.getTitle())) {
            return manageDabWarningOnTodoSubpage(page, pageRevId, text, todoSubpage, talkPage);
          }
        }
      }
    }

    return manageDabWarningOnTalkPage(page, pageRevId, text, talkPage);
  }

  /**
   * Update disambiguation warning on the todo subpage.
   * 
   * @param page Page (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param text Page contents (can be different than the current page text).
   * @param todoSubpage Todo subpage.
   * @param talkPage Talk page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTodoSubpage(
      Page page, Integer pageRevId, String text,
      Page todoSubpage, Page talkPage) throws APIException {
    Collection<String> dabLinks = findDabLinks(page, text);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result |= removeDabWarningOnTodoSubpage(todoSubpage);
      result |= removeDabWarningOnTalkPage(talkPage);
    } else {
      result |= updateDabWarningOnTodoSubpage(pageRevId, todoSubpage, dabLinks);
      if (createWarning) {
        result |= cleanDabWarningOnTalkPage(talkPage, dabLinks);
      }
    }
    return result;
  }

  /**
   * Update disambiguation warning on the talk page.
   * 
   * @param page Page (must have enough information to compute the list of disambiguation links).
   * @param pageRevId Page revision id.
   * @param text Page contents (can be different than the current page text).
   * @param talkPage Talk page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTalkPage(
      Page page, Integer pageRevId, String text, Page talkPage) throws APIException {
    Collection<String> dabLinks = findDabLinks(page, text);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result = removeDabWarningOnTalkPage(talkPage);
    } else {
      result |= updateDabWarningOnTalkPage(pageRevId, talkPage, dabLinks);
    }
    return result;
  }

  /**
   * Create/update disambiguation warning on the todo subpage.
   * 
   * @param pageRevId Page revision id.
   * @param todoSubpage Todo subpage.
   * @param dabLinks List of existing dab links.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean updateDabWarningOnTodoSubpage(
      Integer pageRevId, Page todoSubpage, Collection<String> dabLinks) throws APIException {
    if ((todoSubpage == null) || (dabLinks == null)) {
      return false;
    }

    // Search disambiguation warning in the todo subpage
    String contents = todoSubpage.getContents();
    if (contents == null) {
      contents = "";
    }
    PageElementTemplate templateWarning = PageContents.findNextTemplate(
        todoSubpage, contents, wikipedia.getDisambiguationWarningTemplate(), 0);

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
      api.updatePage(
          wikipedia, todoSubpage, tmp.toString(),
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
      if (index < contents.length()) {
        tmp.append(contents.substring(index));
      }
      api.updatePage(
          wikipedia, todoSubpage, tmp.toString(),
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          false);
      return true;
    }

    return false;
  }

  /**
   * Create/update disambiguation warning on the talk page.
   * 
   * @param pageRevId Page revision id.
   * @param talkPage Talk page.
   * @param dabLinks List of existing dab links.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean updateDabWarningOnTalkPage(
      Integer pageRevId, Page talkPage, Collection<String> dabLinks) throws APIException {
    if ((talkPage == null) || (dabLinks == null)) {
      return false;
    }

    // Search todo template in the talk page
    String contents = talkPage.getContents();
    if (contents == null) {
      contents = "";
    }
    PageElementTemplate templateTodo = null;
    if ((wikipedia.getTodoTemplates() == null) ||
        (wikipedia.getTodoTemplates().isEmpty())) {
      return false;
    }
    for (String todoTemplate : wikipedia.getTodoTemplates()) {
      PageElementTemplate templateTmp = PageContents.findNextTemplate(
          talkPage, contents, todoTemplate, 0);
      if (templateTmp != null) {
        if ((templateTodo == null) || (templateTmp.getBeginIndex() < templateTodo.getBeginIndex())) {
          templateTodo = templateTmp;
        }
      }
    }

    // If todo template is missing, add it
    if (templateTodo == null) {
      if (!createWarning) {
        return false;
      }

      // Search where to add todo template
      PageElementTemplate templatePrevious = null;
      if (wikipedia.getDisambiguationWarningAfterTemplates() != null) {
        for (String previousTemplate : wikipedia.getDisambiguationWarningAfterTemplates()) {
          int index = 0;
          while (index < contents.length()) {
            PageElementTemplate templateTmp = PageContents.findNextTemplate(
                talkPage, contents, previousTemplate, index);
            if (templateTmp != null) {
              if ((templatePrevious == null) || (templateTmp.getEndIndex() > templatePrevious.getEndIndex())) {
                templatePrevious = templateTmp;
              }
            } else {
              index = contents.length();
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
      tmp.append(wikipedia.getTodoTemplates().get(0));
      tmp.append("|* ");
      addWarning(tmp, pageRevId, dabLinks);
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      api.updateSection(
          wikipedia, talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
      return true;
    }

    // Search disambiguation warning in the todo parameter
    String parameter = templateTodo.getParameterValue("1");
    PageElementTemplate templateWarning = PageContents.findNextTemplate(
        talkPage, parameter,
        wikipedia.getDisambiguationWarningTemplate(), 0);
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
      api.updateSection(
          wikipedia, talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
      return true;
    }

    // TODO

    return false;
  }

  /**
   * Remove disambiguation warning on the todo subpage.
   * 
   * @param todoSubpage Todo subpage.
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

    // Search disambiguation warning in the todo subpage
    PageElementTemplate template = PageContents.findNextTemplate(
        todoSubpage, contents, wikipedia.getDisambiguationWarningTemplate(), 0);
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
    api.updatePage(
        wikipedia, todoSubpage, tmp.toString(),
        wikipedia.formatComment(wikipedia.getDisambiguationWarningCommentDone()),
        false);

    return true;
  }

  /**
   * Remove disambiguation warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @param dabLinks Links to dab pages.
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
      api.updateSection(
          wikipedia, talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, "{{" + wikipedia.getTodoTemplates().get(0) + "}}", false);
      return true;
    }

    String contents = talkPage.getContents();
    if (contents == null) {
      return false;
    }

    // Search todo in the talk page
    PageElementTemplate templateTodo = null;
    if (wikipedia.getTodoTemplates() != null) {
      for (String templateName : wikipedia.getTodoTemplates()) {
        PageElementTemplate templateTmp = PageContents.findNextTemplate(
            talkPage, contents, templateName, 0);
        if (templateTmp != null) {
          templateTodo = templateTmp;
        }
      }
    }

    // If template is missing, verify that a link to the todo subpage exists
    if (templateTodo == null) {
      PageElementTemplate templateTodoLink = null;
      if (wikipedia.getTodoLinkTemplates() != null) {
        for (String todoLink : wikipedia.getTodoLinkTemplates()) {
          PageElementTemplate templateTmp = PageContents.findNextTemplate(
              talkPage, contents, todoLink, 0);
          if (templateTmp != null) {
            templateTodoLink = templateTmp;
          }
        }
      }

      // If link exists, nothing more to do
      if (templateTodoLink != null) {
        return false;
      }

      // Search where to add todo template
      PageElementTemplate templatePrevious = null;
      if (wikipedia.getDisambiguationWarningAfterTemplates() != null) {
        for (String previousTemplate : wikipedia.getDisambiguationWarningAfterTemplates()) {
          int index = 0;
          while (index < contents.length()) {
            PageElementTemplate templateTmp = PageContents.findNextTemplate(
                talkPage, contents, previousTemplate, index);
            if (templateTmp != null) {
              if ((templatePrevious == null) || (templateTmp.getEndIndex() > templatePrevious.getEndIndex())) {
                templatePrevious = templateTmp;
              }
              index = templateTmp.getEndIndex();
            } else {
              index = contents.length();
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
      tmp.append(wikipedia.getTodoTemplates().get(0));
      tmp.append("}}");
      if (indexStart < contents.length()) {
        if (contents.charAt(indexStart) != '\n') {
          tmp.append("\n");
        }
        tmp.append(contents.substring(indexStart));
      }
      api.updateSection(
          wikipedia, talkPage,
          wikipedia.formatComment(getDisambiguationWarningComment(dabLinks)),
          0, tmp.toString(), false);
      return true;
    }
    if (templateTodo.getParameterValue("1") == null) {
      return false;
    }

    // Search disambiguation warning in the todo parameter
    String parameter = templateTodo.getParameterValue("1");
    PageElementTemplate templateWarning = PageContents.findNextTemplate(
        talkPage, parameter,
        wikipedia.getDisambiguationWarningTemplate(), 0);
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
        // Search todo link
        PageElementTemplate templateTodoLink = null;
        if (wikipedia.getTodoLinkTemplates() != null) {
          for (String templateName : wikipedia.getTodoLinkTemplates()) {
            PageElementTemplate templateTmp = PageContents.findNextTemplate(
                talkPage, contents, templateName, 0);
            if (templateTmp != null) {
              templateTodoLink = templateTmp;
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
      api.updateSection(
          wikipedia, talkPage,
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

    // Search todo in the talk page
    PageElementTemplate templateTodo = null;
    if (wikipedia.getTodoTemplates() != null) {
      for (String templateName : wikipedia.getTodoTemplates()) {
        PageElementTemplate templateTmp = PageContents.findNextTemplate(
            talkPage, contents, templateName, 0);
        if (templateTmp != null) {
          templateTodo = templateTmp;
        }
      }
    }
    if ((templateTodo != null) && (templateTodo.getParameterValue("1") != null)) {
      // Search disambiguation warning in the todo parameter
      String parameter = templateTodo.getParameterValue("1");
      PageElementTemplate templateWarning = PageContents.findNextTemplate(
          talkPage, parameter,
          wikipedia.getDisambiguationWarningTemplate(), 0);
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
        api.updateSection(
            wikipedia, talkPage,
            wikipedia.formatComment(wikipedia.getDisambiguationWarningCommentDone()),
            0, tmp.toString(), false);
        return true;
      }
    }

    return false;
  }

  /**
   * Extract links to disambiguation pages.
   * 
   * @param page Page (must have enough information to compute the list of disambiguation links).
   * @param text Page contents (can be different than the current page text).
   * @return List of links to disambiguation pages.
   */
  private Collection<String> findDabLinks(Page page, String text) {
    if ((page == null) || (page.getContents() == null)) {
      return null;
    }
    Map<String, Integer> linkCount = PageContents.countInternalDisambiguationLinks(
        wikipedia, page, text, page.getLinks());
    List<String> dabLinks = new ArrayList<String>(linkCount.keySet());
    Collections.sort(dabLinks);
    return dabLinks;
  }

  /**
   * @param dabLinks Links to dab pages.
   * @return Comment.
   */
  private String getDisambiguationWarningComment(Collection<String> dabLinks) {
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      return wikipedia.getDisambiguationWarningComment();
    }
    StringBuilder result = new StringBuilder(wikipedia.getDisambiguationWarningComment());
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
   * Tell if the template shoud be modified.
   * 
   * @param dabLinks Links to dab pages.
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
    talkText.append(wikipedia.getDisambiguationWarningTemplate());
    if (pageRevId != null) {
      talkText.append(" | revisionid=");
      talkText.append(pageRevId);
    }
    for (String dabLink : dabLinks) {
      talkText.append(" | ");
      talkText.append(dabLink);
    }
    talkText.append(" }}");
    if (wikipedia.getDisambiguationWarningTemplateComment() != null) {
      talkText.append(" <!-- ");
      talkText.append(wikipedia.getDisambiguationWarningTemplateComment());
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
