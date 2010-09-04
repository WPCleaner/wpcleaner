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
    if ((updateWarning) && (getWikipedia().getDisambiguationWarningTemplate() != null)) {
      Page talkPage = page.getTalkPage(getWikipedia().getNamespaces());
      try {
        setText(GT._("Retrieving first section contents - {0}", talkPage.getTitle()));
        api.retrieveSectionContents(getWikipedia(), talkPage, 0);
        PageElementTemplate templateWarning = PageContents.findNextTemplate(
            talkPage, talkPage.getContents(),
            getWikipedia().getDisambiguationWarningTemplate(),
            0);
        Map<String, Integer> linkCount = PageContents.countInternalDisambiguationLinks(
            getWikipedia(), page, text, page.getLinks());
        List<String> dabLinks = new ArrayList<String>(linkCount.keySet());
        Collections.sort(dabLinks);
        if (templateWarning == null) {

          // Template warning absent
          if (dabLinks.isEmpty()) {

            // No dab links : nothing to do
          } else {

            // Dab links : warning template must be added
            StringBuilder talkText = new StringBuilder();
            talkText.append("{{ ");
            talkText.append(getWikipedia().getDisambiguationWarningTemplate());
            if ((queryResult != null) && (queryResult.getPageNewRevId() != null)) {
              talkText.append(" | revisionid=");
              talkText.append(queryResult.getPageNewRevId());
            }
            for (String dabLink : dabLinks) {
              talkText.append(" | ");
              talkText.append(dabLink);
            }
            talkText.append(" }}");
            if ((talkPage.getContents() != null) &&
                (talkPage.getContents().trim().length() > 0)) {
              talkText.append("\n");
              talkText.append(talkPage.getContents().trim());
            }
            updateDisambiguationWarning(talkPage, talkText.toString());
          }
        } else {

          // Template warning already existing
          if (dabLinks.isEmpty()) {

            // No dab links : warning template must be removed
            String talkPageContents = talkPage.getContents();
            StringBuilder talkText = new StringBuilder();
            if (templateWarning.getBeginIndex() > 0) {
              talkText.append(talkPageContents.substring(0, templateWarning.getBeginIndex()));
            }
            if (templateWarning.getEndIndex() < talkPageContents.length()) {
              talkText.append(talkPageContents.substring(templateWarning.getEndIndex()));
            }
            updateDisambiguationWarning(talkPage, talkText.toString());
          } else {

            // Dab links : warning template must be updated (if necessary)
            List<String> oldList = new ArrayList<String>();
            for (int i = 0; i < templateWarning.getParameterCount(); i++) {
              String paramValue = templateWarning.getParameterValue(Integer.toString(i + 1));
              if (paramValue != null) {
                oldList.add(paramValue);
              }
            }
            boolean sameList = true;
            if (oldList.size() != dabLinks.size()) {
              sameList = false;
            } else {
              for (int i = 0; i < oldList.size(); i++) {
                if (!oldList.get(i).equals(dabLinks.get(i))) {
                  sameList = false;
                }
              }
            }
            if (!sameList) {
              String talkPageContents = talkPage.getContents();
              StringBuilder talkText = new StringBuilder();
              if (templateWarning.getBeginIndex() > 0) {
                talkText.append(talkPageContents.substring(0, templateWarning.getBeginIndex()));
              }
              talkText.append("{{ ");
              talkText.append(getWikipedia().getDisambiguationWarningTemplate());
              if ((queryResult != null) && (queryResult.getPageNewRevId() != null)) {
                talkText.append(" | revisionid=");
                talkText.append(queryResult.getPageNewRevId());
              }
              for (String dabLink : dabLinks) {
                talkText.append(" | ");
                talkText.append(dabLink);
              }
              talkText.append(" }}");
              if (templateWarning.getEndIndex() < talkPageContents.length()) {
                talkText.append(talkPageContents.substring(templateWarning.getEndIndex()));
              }
              updateDisambiguationWarning(talkPage, talkText.toString());
            }
          }
        }
      } catch (APIException e) {
        return e;
      }
    }
    return null;
  }

  private void updateDisambiguationWarning(
      Page talkPage, String talkText) throws APIException {
    setText(GT._("Updating disambiguation warning on talk page"));
    API api = APIFactory.getAPI();
   try {
      api.updateSection(
          getWikipedia(), talkPage,
          getWikipedia().formatComment(getWikipedia().getDisambiguationWarningTemplate()), 0,
          talkText, forceWatch);
    } catch (APIException e) {
      if (APIException.ERROR_BAD_TOKEN.equals(e.getErrorCode())) {
        setText(GT._("Error 'badtoken' detected: Retrying"));
        api.updateSection(
            getWikipedia(), talkPage,
            getWikipedia().formatComment(getWikipedia().getDisambiguationWarningTemplate()), 0,
            talkText, forceWatch);
      } else {
        throw e;
      }
    }
  }
}