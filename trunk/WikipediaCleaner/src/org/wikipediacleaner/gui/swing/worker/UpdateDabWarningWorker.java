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

  private final String start;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param start Start at this page.
   */
  public UpdateDabWarningWorker(EnumWikipedia wikipedia, BasicWindow window, String start) {
    super(wikipedia, window);
    this.start = (start != null) ? start.trim() : "";
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    EnumWikipedia wikipedia = getWikipedia();

    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();
    int count = 0;
    int lastCount = count;

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
      UpdateDabWarningTools tools = new UpdateDabWarningTools(wikipedia, this);
      while (!dabWarningPages.isEmpty()) {
        // Creating sublist
        int size = Math.min(10, dabWarningPages.size());
        List<Page> sublist = new ArrayList<Page>(size);
        while ((sublist.size() < size) && (dabWarningPages.size() > 0)) {
          Page page = dabWarningPages.remove(0);
          if ((start.length() == 0) || (start.compareTo(page.getTitle()) < 0)) {
            sublist.add(page);
          }
        }
        if (sublist.isEmpty()) {
          return Integer.valueOf(count);
        }

        // Update disambiguation warning
        count += tools.updateDabWarning(sublist);
        if (shouldStop()) {
          return Integer.valueOf(count);
        }

        if (count > lastCount) {
          lastCount = count;
          /*if (getWindow() != null) {
            int answer = getWindow().displayYesNoWarning(
                "This feature is currently under development, please check the modification.\n" +
                "Do you want to continue ?");
            if (answer != JOptionPane.YES_OPTION) {
              return Integer.valueOf(count);
            }
          } else {
            return Integer.valueOf(count);
          }*/
        }
      }
    } catch (APIException e) {
      return e;
    }

    return Integer.valueOf(count);
  }
}
