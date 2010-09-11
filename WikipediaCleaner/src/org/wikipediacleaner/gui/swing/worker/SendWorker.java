/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for sending the new page content. 
 */
public class SendWorker extends BasicWorker {

  private final Page page;
  private final String text;
  private final String comment;
  private final boolean forceWatch;
  private final boolean updateWarning;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   * @param text Page contents.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @param updateWarning Update warning on talk page.
   */
  public SendWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      Page page, String text, String comment,
      boolean forceWatch, boolean updateWarning) {
    super(wikipedia, window);
    this.page = page;
    this.text = text;
    this.comment = comment;
    this.forceWatch = forceWatch;
    this.updateWarning = updateWarning;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();

    // Updating page contents
    QueryResult queryResult = null;
    try {
      setText(GT._("Updating page contents"));
      queryResult = api.updatePage(
          getWikipedia(), page, text,
          getWikipedia().createUpdatePageComment(comment, null),
          forceWatch);
    } catch (APIException e) {
      if (APIException.ERROR_BAD_TOKEN.equals(e.getErrorCode())) {
        try {
          setText(GT._("Error 'badtoken' detected: Retrying"));
          api.retrieveContents(getWikipedia(), page, false);
          queryResult = api.updatePage(
              getWikipedia(), page, text,
              getWikipedia().createUpdatePageComment(comment, null),
              forceWatch);
        } catch (APIException e2) {
          return e2;
        }
      } else {
        return e;
      }
    }

    // Updating disambiguation warning
    String dabWarningTemplate = getWikipedia().getDisambiguationWarningTemplate();
    List<String> todoTemplates = getWikipedia().getTodoTemplates();
    String todoSubpage = getWikipedia().getTodoSubpage();
    if ((updateWarning) &&
        (dabWarningTemplate != null) &&
        (todoTemplates != null) &&
        (!todoTemplates.isEmpty()) &&
        (todoSubpage != null)) {
      Page talkPage = page.getTalkPage(getWikipedia().getNamespaces());
      try {

        // Retrieving first section of talk page
        setText(GT._("Retrieving first section contents - {0}", talkPage.getTitle()));
        api.retrieveSectionContents(getWikipedia(), talkPage, 0);
        String talkPageContents = talkPage.getContents();
        if (talkPageContents == null) {
          talkPageContents = "";
        }

        // Analyzing first section of talk page
        PageElementTemplate templateTodo = null;
        PageElementTemplate templateWarningTodo = null;
        PageElementTemplate templateWarning = null;
        int startIndex = 0;
        while ((startIndex < talkPageContents.length()) &&
               (templateWarningTodo == null) &&
               (templateWarning == null)) {
          PageElementTemplate template = PageContents.findNextTemplate(
              talkPage, talkPageContents, startIndex);
          if (template == null) {
            startIndex = talkPageContents.length();
          } else {
            startIndex = template.getEndIndex();
            for (String todoTemplate : todoTemplates) {
              if (Page.areSameTitle(todoTemplate, template.getTemplateName())) {
                templateTodo = template;
                String arg = templateTodo.getParameterValue("1");
                if (arg != null) {
                  templateWarningTodo = PageContents.findNextTemplate(
                      talkPage, arg, dabWarningTemplate, 0);
                }
              }
            }
            if (dabWarningTemplate.equals(template.getTemplateName())) {
              templateWarning = template;
            }
          }
        }

        // Retrieving todo subpage
        Page pageTodoSubpage = talkPage.getSubPage(todoSubpage);
        setText(GT._("Retrieving page contents - {0}", pageTodoSubpage.getTitle()));
        api.retrieveContents(getWikipedia(), pageTodoSubpage, false);

        // Analyzing todo subpage
        PageElementTemplate templateWarningSubpage = null;
        String pageTodoSubpageContents = pageTodoSubpage.getContents();
        if (pageTodoSubpageContents == null) {
          pageTodoSubpageContents = "";
        }
        if (Boolean.TRUE.equals(pageTodoSubpage.isExisting())) {
          templateWarningSubpage = PageContents.findNextTemplate(
              pageTodoSubpage, pageTodoSubpageContents,
              dabWarningTemplate, 0);
        }

        // Analyzing page contents for disambiguation links
        Map<String, Integer> linkCount = PageContents.countInternalDisambiguationLinks(
            getWikipedia(), page, text, page.getLinks());
        List<String> dabLinks = new ArrayList<String>(linkCount.keySet());
        Collections.sort(dabLinks);

        if (dabLinks.isEmpty()) {

          // No dab links
          if (Boolean.TRUE.equals(pageTodoSubpage.isExisting())) {

            // No dab links, todo subpage existing
            if (templateWarningSubpage != null) {

              // No dab links, todo subpage existing, warning existing => Remove warning
              StringBuilder tmp = new StringBuilder();
              if (templateWarningSubpage.getBeginIndex() > 0) {
                tmp.append(pageTodoSubpageContents.substring(0, templateWarningSubpage.getBeginIndex()));
              }
              if (templateWarningSubpage.getEndIndex() < pageTodoSubpageContents.length()) {
                tmp.append(pageTodoSubpageContents.substring(templateWarningSubpage.getEndIndex()));
              }
              updateDisambiguationWarning(pageTodoSubpage, tmp.toString(), false);
            }

          } else {

            // No dab links, no todo subpage
            if ((templateTodo != null) && (templateWarningTodo != null)) {

              // No dab links, no todo subpage, warning existing in todo => Remove warning
              StringBuilder tmp = new StringBuilder();
              if (templateTodo.getBeginIndex() > 0) {
                tmp.append(talkPageContents.substring(0, templateTodo.getBeginIndex()));
              }
              int beginIndex = templateWarningTodo.getBeginIndex();
              String argText = templateTodo.getParameterValue("1");
              while ((beginIndex > 0) && (argText.charAt(beginIndex - 1) == ' ')) {
                beginIndex--;
              }
              if ((beginIndex > 0) && (argText.charAt(beginIndex - 1) == '*')) {
                beginIndex--;
              }
              if ((beginIndex > 0) && (argText.charAt(beginIndex - 1) == '\n')) {
                beginIndex--;
              }
              int endIndex = templateWarningTodo.getEndIndex();
              if ((beginIndex > 0) || (endIndex < argText.length())) {
                StringBuilder tmpArgText = new StringBuilder();
                if (beginIndex > 0) {
                  tmpArgText.append(argText.substring(0, beginIndex));
                }
                if (endIndex < argText.length()) {
                  tmpArgText.append(argText.substring(endIndex));
                }
                tmp.append(templateTodo.getParameterReplacement("1", tmpArgText.toString(), null));
              }
              if (templateTodo.getEndIndex() < talkPageContents.length()) {
                tmp.append(talkPageContents.substring(templateTodo.getEndIndex()));
              }
              updateDisambiguationWarning(talkPage, tmp.toString(), true);

            } else if (templateWarning != null) {

              // No dab links, no todo subpage, warning existing => Remove warning
              StringBuilder tmp = new StringBuilder();
              if (templateWarning.getBeginIndex() > 0) {
                tmp.append(talkPageContents.substring(0, templateWarning.getBeginIndex()));
              }
              if (templateWarning.getEndIndex() < talkPageContents.length()) {
                tmp.append(talkPageContents.substring(templateWarning.getEndIndex()));
              }
              updateDisambiguationWarning(talkPage, tmp.toString(), true);
            }
          }
        } else {

          // Dab links existing
          if (Boolean.TRUE.equals(pageTodoSubpage.isExisting())) {

            // Dab links existing, todo subpage existing
            if (templateWarningSubpage == null) {

              // Dab links existing, todo subpage existing, no template warning => Add warning
              StringBuilder tmp = new StringBuilder();
              if ((pageTodoSubpageContents.trim().length() > 0)) {
                tmp.append(pageTodoSubpageContents.trim());
                tmp.append("\n");
              }
              tmp.append("* ");
              addWarning(tmp, queryResult.getPageNewRevId(), dabLinks);
              updateDisambiguationWarning(
                  pageTodoSubpage, tmp.toString(), false);

            } else {

              // Dab links existing, todo subpage existing, template warning existing => Update warning
              if (!areSameList(dabLinks, templateWarningSubpage)) {
                StringBuilder tmp = new StringBuilder();
                if (templateWarningSubpage.getBeginIndex() > 0) {
                  tmp.append(pageTodoSubpageContents.substring(0, templateWarningSubpage.getBeginIndex()));
                }
                addWarning(tmp, queryResult.getPageNewRevId(), dabLinks);
                if (templateWarningSubpage.getEndIndex() < pageTodoSubpageContents.length()) {
                  tmp.append(pageTodoSubpageContents.substring(templateWarningSubpage.getEndIndex()));
                }
                updateDisambiguationWarning(
                    pageTodoSubpage, tmp.toString(), false);
              }
            }

          } else {

            // Dab links existing, no todo subpage
            if ((templateTodo != null) && (templateWarningTodo != null)) {

              // Dab link existing, no todo subpage, warning existing in todo => Update warning
              StringBuilder tmp = new StringBuilder();
              int index =
                templateTodo.getBeginIndex() +
                templateTodo.getParameterValueOffset(0);
              tmp.append(talkPageContents.substring(0, index + templateWarningTodo.getBeginIndex()));
              addWarning(tmp, queryResult.getPageNewRevId(), dabLinks);
              tmp.append(talkPageContents.substring(index + templateWarningTodo.getEndIndex()));
              updateDisambiguationWarning(talkPage, tmp.toString(), true);

            } else if (templateWarning != null) {

              // Dab link existing, no todo subpage, warning existing => Update warning
              StringBuilder tmp = new StringBuilder();
              if (templateWarning.getBeginIndex() > 0) {
                tmp.append(talkPageContents.substring(0, templateWarning.getBeginIndex()));
              }
              addWarning(tmp, queryResult.getPageNewRevId(), dabLinks);
              if (templateWarning.getEndIndex() < talkPageContents.length()) {
                tmp.append(talkPageContents.substring(templateWarning.getEndIndex()));
              }
              updateDisambiguationWarning(talkPage, tmp.toString(), true);

            } else {

              // Dab link existing, no todo subpage, no warning => Add warning
              StringBuilder tmp = new StringBuilder();
              tmp.append("{{");
              tmp.append(todoTemplates.get(0));
              tmp.append("|* ");
              addWarning(tmp, queryResult.getPageNewRevId(), dabLinks);
              tmp.append(" }}");
              if (talkPageContents.trim().length() > 0) {
                tmp.append("\n");
                tmp.append(talkPageContents.trim());
              }
              updateDisambiguationWarning(talkPage, tmp.toString(), true);
            }
          }
        }
      } catch (APIException e) {
        return e;
      }
    }
    return null;
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
      Integer pageRevId, List<String> dabLinks) {
    talkText.append("{{");
    talkText.append(getWikipedia().getDisambiguationWarningTemplate());
    if (pageRevId != null) {
      talkText.append("|revisionid=");
      talkText.append(pageRevId);
    }
    for (String dabLink : dabLinks) {
      talkText.append("|");
      talkText.append(dabLink);
    }
    talkText.append("}}");
  }

  /**
   * Check if the list of disambiguation links is the same.
   * 
   * @param dabLinks Original list.
   * @param templateWarning Template warning.
   * @return True if both lists are identical.
   */
  private boolean areSameList(List<String> dabLinks, PageElementTemplate templateWarning) {
    List<String> oldList = new ArrayList<String>();
    for (int i = 0; i < templateWarning.getParameterCount(); i++) {
      String paramValue = templateWarning.getParameterValue(Integer.toString(i + 1));
      if (paramValue != null) {
        oldList.add(paramValue);
      }
    }
    if (oldList.size() != dabLinks.size()) {
      return false;
    }
    for (int i = 0; i < oldList.size(); i++) {
      if (!oldList.get(i).equals(dabLinks.get(i))) {
        return false;
      }
    }
    return true;
  }

  /**
   * Update disambiguation warning.
   * 
   * @param updatePage Updated page.
   * @param updateText Updated text.
   * @param onlySection Only first section or full page ?
   * @throws APIException
   */
  private void updateDisambiguationWarning(
      Page updatePage, String updateText, boolean onlySection) throws APIException {
    setText(GT._("Updating disambiguation warning - {0}", updatePage.getTitle()));
    API api = APIFactory.getAPI();
   try {
     if (onlySection) {
       api.updateSection(
            getWikipedia(), updatePage,
            getWikipedia().formatComment(getWikipedia().getDisambiguationWarningComment()),
            0, updateText, forceWatch);
     } else {
       api.updatePage(
           getWikipedia(), updatePage, updateText,
           getWikipedia().formatComment(getWikipedia().getDisambiguationWarningComment()),
           false);
     }
    } catch (APIException e) {
      if (APIException.ERROR_BAD_TOKEN.equals(e.getErrorCode())) {
        if (onlySection) {
          api.updateSection(
               getWikipedia(), updatePage,
               getWikipedia().formatComment(getWikipedia().getDisambiguationWarningComment()),
               0, updateText, forceWatch);
        } else {
          api.updatePage(
              getWikipedia(), updatePage, updateText,
              getWikipedia().formatComment(getWikipedia().getDisambiguationWarningComment()),
              false);
        }
      } else {
        throw e;
      }
    }
  }
}