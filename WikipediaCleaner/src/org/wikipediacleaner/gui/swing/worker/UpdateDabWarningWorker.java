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
import java.util.List;

import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for updating disambiguation warning.
 */
public class UpdateDabWarningWorker extends BasicWorker {

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   */
  public UpdateDabWarningWorker(EnumWikipedia wikipedia, BasicWindow window) {
    super(wikipedia, window);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    EnumWikipedia wikipedia = getWikipedia();

    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();

    try {
      // Retrieve talk pages including a disambiguation warning
      String dabWarningTemplateName = wikipedia.getDisambiguationWarningTemplate();
      setText(GT._("Retrieving talk pages including {0}", "{{" + dabWarningTemplateName + "}}"));
      Page dabWarningTemplate = DataManager.getPage(
          wikipedia,
          Namespace.getTitle(Namespace.TEMPLATE, wikipedia.getNamespaces(), dabWarningTemplateName),
          null, null);
      api.retrieveEmbeddedIn(wikipedia, dabWarningTemplate, Namespace.MAIN_TALK);

      // Construct list of articles with disambiguation warning
      setText(GT._("Constructing list of articles with disambiguation warning"));
      Namespace namespaceTalk = Namespace.getNamespace(Namespace.MAIN_TALK, wikipedia.getNamespaces());
      List<Page> dabWarningTalkPages = dabWarningTemplate.getEmbeddedIn();
      List<Page> dabWarningPages = new ArrayList<Page>();
      for (Page dabWarningPage : dabWarningTalkPages) {
        String title = dabWarningPage.getTitle();
        int colonIndex = title.indexOf(':');
        if (colonIndex >= 0) {
          if (namespaceTalk.isPossibleName(title.substring(0, colonIndex))) {
            title = title.substring(colonIndex + 1);
          }
        }
        if (title.endsWith("/" + wikipedia.getTodoSubpage())) {
          title = title.substring(0, title.length() - 1 - wikipedia.getTodoSubpage().length());
        }
        Page page = DataManager.getPage(wikipedia, title, null, null);
        if (!dabWarningPages.contains(page)) {
          dabWarningPages.add(page);
        }
      }

      // Working with sublists
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      List<Page> knownPages = new ArrayList<Page>();
      while (!dabWarningPages.isEmpty()) {
        // Creating sublist
        int size = Math.min(10, dabWarningPages.size());
        List<Page> sublist = new ArrayList<Page>(size);
        for (int i = 0; i < size; i++) {
          sublist.add(dabWarningPages.remove(0));
        }

        // Retrieving links in each page
        for (Page dabWarningPage : sublist) {
          mw.retrieveAllLinks(wikipedia, dabWarningPage, Namespace.MAIN, null, false);
        }
        mw.block(true);
        if (shouldStop()) {
          return null;
        }
  
        // Retrieving disambiguation information in each page
        for (Page dabWarningPage : sublist) {
          mw.retrieveDisambiguationInformation(wikipedia, dabWarningPage.getLinks(), knownPages, true, false);
        }
        mw.block(true);
        if (shouldStop()) {
          return null;
        }

        // TODO
        if (getWindow() != null) {
          getWindow().displayWarning("This feature is currently under development");
        }
        return null;
      }
    } catch (APIException e) {
      return e;
    }

    return null;
  }

}
