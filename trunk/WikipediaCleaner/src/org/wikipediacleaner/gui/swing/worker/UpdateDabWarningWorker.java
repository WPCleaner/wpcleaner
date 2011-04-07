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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for updating disambiguation warning.
 */
public class UpdateDabWarningWorker extends BasicWorker {

  private final String start;
  private final List<Page> dabWarningPages;
  private final boolean useList;
  private final boolean contentsAvailable;
  private final boolean linksAvailable;
  private final boolean dabInformationAvailable;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param start Start at this page.
   */
  public UpdateDabWarningWorker(EnumWikipedia wikipedia, BasicWindow window, String start) {
    super(wikipedia, window);
    this.start = (start != null) ? start.trim() : "";
    this.dabWarningPages = new ArrayList<Page>();
    this.useList = false;
    this.contentsAvailable = false;
    this.linksAvailable = false;
    this.dabInformationAvailable = false;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages to analyze.
   */
  public UpdateDabWarningWorker(
      EnumWikipedia wikipedia, BasicWindow window, List<Page> pages) {
    this(wikipedia, window, pages, false, false, false);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param linksAvailable True if links are already available in pages.
   * @param dabInformationAvailable True if dab information is already available in pages.
   */
  public UpdateDabWarningWorker(
      EnumWikipedia wikipedia, BasicWindow window, List<Page> pages,
      boolean contentsAvailable, boolean linksAvailable, boolean dabInformationAvailable) {
    super(wikipedia, window);
    this.start = "";
    this.dabWarningPages = new ArrayList<Page>(pages);
    this.useList = true;
    this.contentsAvailable = contentsAvailable;
    this.linksAvailable = linksAvailable;
    this.dabInformationAvailable = dabInformationAvailable;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    long startTime = System.currentTimeMillis();
    EnumWikipedia wikipedia = getWikipedia();

    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();
    int countAnalyzed = 0;
    int countUpdated = 0;
    int lastCount = countUpdated;

    try {
      if (!useList) {
        // Retrieve talk pages including a disambiguation warning
        String dabWarningTemplateName = wikipedia.getDisambiguationWarningTemplate();
        setText(GT._("Retrieving talk pages including {0}", "{{" + dabWarningTemplateName + "}}"));
        Page dabWarningTemplate = DataManager.getPage(
            wikipedia,
            Namespace.getTitle(Namespace.TEMPLATE, wikipedia.getNamespaces(), dabWarningTemplateName),
            null, null);
        List<Page> dabWarningTalkPages = api.retrieveEmbeddedIn(
            wikipedia, dabWarningTemplate, wikipedia.getEncyclopedicTalkNamespaces());
  
        // Construct list of articles with disambiguation warning
        setText(GT._("Constructing list of articles with disambiguation warning"));
        HashSet<Page> tmpWarningPages = new HashSet<Page>();
        for (Page dabWarningPage : dabWarningTalkPages) {
          String title = dabWarningPage.getTitle();
          if (title.endsWith("/" + wikipedia.getTodoSubpage())) {
            title = title.substring(0, title.length() - 1 - wikipedia.getTodoSubpage().length());
          }
          int colonIndex = title.indexOf(':');
          if (colonIndex >= 0) {
            for (Integer namespace : wikipedia.getEncyclopedicTalkNamespaces()) {
              Namespace namespaceTalk = Namespace.getNamespace(namespace, wikipedia.getNamespaces());
              if ((namespaceTalk != null) &&
                  (namespaceTalk.isPossibleName(title.substring(0, colonIndex)))) {
                String tmpTitle = title.substring(colonIndex + 1);
                if (namespace != Namespace.MAIN_TALK) {
                  tmpTitle = Namespace.getTitle(namespace - 1, wikipedia.getNamespaces(), tmpTitle);
                }
                Page page = DataManager.getPage(wikipedia, tmpTitle, null, null);
                if (!tmpWarningPages.contains(page)) {
                  tmpWarningPages.add(page);
                }
              }
            }
          }
        }
        dabWarningPages.addAll(tmpWarningPages);
        tmpWarningPages.clear();

        // Sort the list of articles
        Collections.sort(dabWarningPages, PageComparator.getTitleFirstComparator());
        for (int pos = dabWarningPages.size() - 1; pos >= 0; pos--) {
          if ((start.length() != 0) && (start.compareTo(dabWarningPages.get(pos).getTitle()) >= 0)) {
            dabWarningPages.remove(pos);
          }
        }
        if (dabWarningPages.isEmpty()) {
          return Integer.valueOf(0);
        }
        if (getWindow() != null) {
          int answer = getWindow().displayYesNoWarning(GT._(
              "Analysis found {0} articles with disambiguation warning {1}.\n" +
              "Do you want to update the disambiguation warnings ?",
              new Object[] { Integer.valueOf(dabWarningPages.size()), "{{" + dabWarningTemplateName + "}}" }));
          if (answer != JOptionPane.YES_OPTION) {
            return Integer.valueOf(0);
          }
        }
      }

      // Working with sublists
      UpdateDabWarningTools tools = new UpdateDabWarningTools(wikipedia, this);
      if (!useList) {
        setText(GT._("Retrieving disambiguation pages"));
        tools.preloadDabPages();
      }
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
          return Integer.valueOf(countUpdated);
        }

        // Update disambiguation warning
        boolean finish = false;
        while (!finish) {
          finish = true;
          try {
            countUpdated += tools.updateDabWarning(sublist, contentsAvailable, linksAvailable, dabInformationAvailable);
            countAnalyzed += sublist.size();
          } catch (APIException e) {
            if (getWindow() != null) {
              int answer = getWindow().displayYesNoWarning(GT._(
                  "An error occured when updating disambiguation warnings. Do you want to continue ?\n\n" +
                  "Error: {0}", e.getMessage()));
              if (answer != JOptionPane.YES_OPTION) {
                return e;
              }
              finish = false;
            }
          }
          if (shouldStop()) {
            return Integer.valueOf(countUpdated);
          }
        }

        if (countUpdated > lastCount) {
          lastCount = countUpdated;
          /*if (getWindow() != null) {
            int answer = getWindow().displayYesNoWarning(
                "This feature is currently under development, please check the modification.\n" +
                "Do you want to continue ?");
            if (answer != JOptionPane.YES_OPTION) {
              return Integer.valueOf(lastCount);
            }
          } else {
            return Integer.valueOf(lastCount);
          }*/
        }
      }
    } catch (APIException e) {
      return e;
    }

    long endTime = System.currentTimeMillis();
    if ((!useList) && (getWindow() != null)) {
      Utilities.displayInformationMessage(
          getWindow().getParentComponent(),
          GT._(
              "Disambiguation links have been analyzed in {0} pages.\n" +
              "The disambiguation warning has been updated in {1} pages.\n" +
              "It took {2} seconds.",
              new Object[] {
                  Integer.valueOf(countAnalyzed),
                  Integer.valueOf(countUpdated),
                  Long.valueOf((endTime - startTime) / 1000) } ));
    }
    return Integer.valueOf(countUpdated);
  }
}
