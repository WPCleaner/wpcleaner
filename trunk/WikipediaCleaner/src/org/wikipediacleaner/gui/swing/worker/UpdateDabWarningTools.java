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
  private final Map<String, Page> dabPages;
  private final Map<String, Page> nonDabPages;
  private final API api;

  /**
   * @param wikipedia Wikipedia.
   */
  public UpdateDabWarningTools(EnumWikipedia wikipedia, BasicWorker worker) {
    this.wikipedia = wikipedia;
    this.worker = worker;
    this.window = (worker != null) ? worker.getWindow() : null;
    this.dabPages = new HashMap<String, Page>();
    this.nonDabPages = new HashMap<String, Page>();
    this.api = APIFactory.getAPI();
  }

  /**
   * @param wikipedia Wikipedia.
   */
  public UpdateDabWarningTools(EnumWikipedia wikipedia, BasicWindow window) {
    this.wikipedia = wikipedia;
    this.worker = null;
    this.window = window;
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

    // Retrieving page contents
    // TODO: page contents is needed only if links to dab pages
    mw.retrieveContents(wikipedia, pages, false, false);

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
    for (Page page : pages) {
      for (int numLink = 0; numLink < page.getLinks().size(); numLink++) {
        Page link = page.getLinks().get(numLink);
        if (dabPages.containsKey(link.getTitle())) {
          page.getLinks().set(numLink, dabPages.get(link.getTitle()));
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
      } else {
        nonDabPages.put(page.getTitle(), page);
      }
    }
    if (shouldStop()) {
      return 0;
    }

    // Update disambiguation warning
    int count = 0;
    for (Page page : pages) {
      if (updateDabWarning(page, page.getContents())) {
        count++;
      }
    }
    return count;
  }

  /**
   * Update disambiguation warning for a page.
   * 
   * @param page Page (must have enough information to compute the list of disambiguation links).
   * @param text Page contents (can be different than the current page text).
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean updateDabWarning(Page page, String text) throws APIException {
    if (page == null) {
      return false;
    }

    // Finding talk page and todo subpage
    Page talkPage = page.getTalkPage(wikipedia.getNamespaces());
    Page todoSubpage = talkPage.getSubPage(wikipedia.getTodoSubpage());

    // Retrieving talk page contents
    // TODO: only section 0 ?
    setText(GT._("Retrieving page contents - {0}", talkPage.getTitle()));
    api.retrieveContents(wikipedia, talkPage, false);

    // Retrieving todo subpage contents
    setText(GT._("Retrieving page contents - {0}", todoSubpage.getTitle()));
    api.retrieveContents(wikipedia, todoSubpage, false);

    // If todo subpage exists, the disambiguation warning must be on it
    if (Boolean.TRUE.equals(todoSubpage.isExisting())) {
      return manageDabWarningOnTodoSubpage(page, page.getRevisionId(), text, todoSubpage, talkPage);
    }

    // If talk page has a link to the todo subpage, the disambiguation warning must be on the todo subpage
    api.retrieveLinks(wikipedia, talkPage, talkPage.getNamespace());
    if (talkPage.getLinks() != null) {
      for (Page link : talkPage.getLinks()) {
        if (Page.areSameTitle(link.getTitle(), todoSubpage.getTitle())) {
          return manageDabWarningOnTodoSubpage(page, page.getRevisionId(), text, todoSubpage, talkPage);
        }
      }
    }

    return manageDabWarningOnTalkPage(todoSubpage, text, talkPage);
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
      result |= cleanDabWarningOnTalkPage(talkPage);
    }
    return result;
  }

  /**
   * Update disambiguation warning on the talk page.
   * 
   * @param page Page (must have enough information to compute the list of disambiguation links).
   * @param text Page contents (can be different than the current page text).
   * @param talkPage Talk page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean manageDabWarningOnTalkPage(
      Page page, String text, Page talkPage) throws APIException {
    Collection<String> dabLinks = findDabLinks(page, text);
    boolean result = false;
    if ((dabLinks == null) || (dabLinks.isEmpty())) {
      result = removeDabWarningOnTalkPage(talkPage);
    } else {
      // TODO
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
          wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()),
          false);
      return true;
    }

    // Check if modifications are needed
    if (isModified(dabLinks, templateWarning)) {
      System.err.println("Modified " + todoSubpage.getTitle());
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
          wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()),
          false);
      return true;
    }

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
        wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()),
        false);

    return true;
  }

  /**
   * Remove disambiguation warning on the talk page.
   * 
   * @param talkPage Talk page.
   * @return True if the disambiguation warning has been updated.
   * @throws APIException
   */
  private boolean cleanDabWarningOnTalkPage(
      Page talkPage) throws APIException {
    // Check if page is already empty
    if ((talkPage == null) || (Boolean.FALSE.equals(talkPage.isExisting()))) {
      return false;
    }
    String contents = talkPage.getContents();
    if (contents == null) {
      contents = "";
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
          tmp.append(templateTodo.getParameterReplacement("1", null, null));
        }
        if (templateTodo.getEndIndex() < contents.length()) {
          tmp.append(contents.substring(templateTodo.getEndIndex()));
        }
        api.updatePage(
            wikipedia, talkPage, tmp.toString(),
            wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()), false);
        return true;
      }
    }

    // Search disambiguation warning in the talk page
    /* It shouldn't happen because it should be included in a todo template
    PageElementTemplate templateWarning = PageContents.findNextTemplate(
        talkPage, contents, wikipedia.getDisambiguationWarningTemplate(), 0);
    if (templateWarning != null) {
      setText(GT._("Removing disambiguation warning - {0}", talkPage.getTitle()));
      System.err.println("Removing disambiguation warning - " + talkPage.getTitle());
      StringBuilder tmp = new StringBuilder();
      int index = templateWarning.getBeginIndex();
      while ((index > 0) && (contents.charAt(index) != '\n')) {
        index--;
      }
      if (index > 0) {
        tmp.append(contents.substring(0, index));
      }
      index = templateWarning.getEndIndex();
      while ((index < contents.length()) && (contents.charAt(index) != '\n')) {
        index++;
      }
      if (index < contents.length()) {
        if (tmp.length() > 0) {
          tmp.append('\n');
        }
        tmp.append(contents.substring(index));
      }
      api.updatePage(
          wikipedia, talkPage, tmp.toString(),
          wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()), false);
      return true;
    }*/

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
      contents = "";
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
        api.updatePage(
            wikipedia, talkPage, tmp.toString(),
            wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()), false);
        return true;
      }
    }

    // Search disambiguation warning in the talk page
    /* It shouldn't happen because it should be included in a todo template
    PageElementTemplate templateWarning = PageContents.findNextTemplate(
        talkPage, contents, wikipedia.getDisambiguationWarningTemplate(), 0);
    if (templateWarning != null) {
      setText(GT._("Removing disambiguation warning - {0}", talkPage.getTitle()));
      System.err.println("Removing disambiguation warning - " + talkPage.getTitle());
      StringBuilder tmp = new StringBuilder();
      int index = templateWarning.getBeginIndex();
      while ((index > 0) && (contents.charAt(index) != '\n')) {
        index--;
      }
      if (index > 0) {
        tmp.append(contents.substring(0, index));
      }
      index = templateWarning.getEndIndex();
      while ((index < contents.length()) && (contents.charAt(index) != '\n')) {
        index++;
      }
      if (index < contents.length()) {
        if (tmp.length() > 0) {
          tmp.append('\n');
        }
        tmp.append(contents.substring(index));
      }
      api.updatePage(
          wikipedia, talkPage, tmp.toString(),
          wikipedia.formatComment(wikipedia.getDisambiguationWarningComment()), false);
      return true;
    }*/

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
    if (page == null) {
      return null;
    }
    Map<String, Integer> linkCount = PageContents.countInternalDisambiguationLinks(
        wikipedia, page, text, page.getLinks());
    List<String> dabLinks = new ArrayList<String>(linkCount.keySet());
    Collections.sort(dabLinks);
    return dabLinks;
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
    talkText.append("{{");
    talkText.append(wikipedia.getDisambiguationWarningTemplate());
    if (pageRevId != null) {
      talkText.append("|revisionid=");
      talkText.append(pageRevId);
    }
    for (String dabLink : dabLinks) {
      talkText.append("|");
      talkText.append(dabLink);
    }
    talkText.append("}}");
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
