/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for checking white lists for Check Wiki.
 */
class CWCheckWhiteListsWorker extends BasicWorker {

  /**
   * @param wiki Wiki.
   * @param window Window.
   */
  public CWCheckWhiteListsWorker(
      EnumWikipedia wiki, BasicWindow window) {
    super(wiki, window);
  }

  /** 
   * Compute the value to be returned by the <code>get</code> method. 
   * 
   * @return Object returned by the <code>get</code> method.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      CheckWiki checkWiki = APIFactory.getCheckWiki();
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      API api = APIFactory.getAPI();
      EnumWikipedia wiki = getWikipedia();
      StringBuilder result = new StringBuilder();
      StringBuilder details = new StringBuilder();
      List<CheckErrorAlgorithm> algorithms = CheckErrorAlgorithms.getAlgorithms(wiki);
      for (CheckErrorAlgorithm algorithm : algorithms) {
        int errorNumber = algorithm.getErrorNumber();
        if (algorithm.isAvailable() &&
            CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
          setText(GT._("Checking whitelist for error {0}", String.valueOf(errorNumber)));
          CWConfigurationError cwConfig = wiki.getCWConfiguration().getErrorConfiguration(errorNumber);
          Set<String> whiteList = cwConfig.getWhiteList();
          if (whiteList != null) {
            List<Page> unnecessaryPages = new ArrayList<>();
            details.setLength(0);
            if (whiteList.size() > 0) {

              // Prepare list of pages to check
              List<Page> pages = new ArrayList<>(whiteList.size());
              for (String pageName : whiteList) {
                Page page = DataManager.getPage(wiki, pageName, null, null, null);
                pages.add(page);
              }
              Collections.sort(pages);
              mw.retrieveContents(wiki, pages, true, false, false, false);

              // Check each page
              for (Page page : pages) {
                if (Boolean.FALSE.equals(page.isExisting())) {
                  details.append("<li>");
                  details.append(GT._("The page {0} doesn''t exist on Wikipedia", page.getTitle()));
                  details.append("</li>");
                  unnecessaryPages.add(page);
                } else {
                  CheckErrorPage errorPage = CheckError.analyzeError(
                      algorithm, page.getAnalysis(page.getContents(), true));
                  if ((errorPage == null) || (!errorPage.getErrorFound())) {
                    details.append("<li>");
                    String pageLink =
                        "<a href=\"" +
                        wiki.getSettings().getURL(page.getTitle(), false, true) +
                        "\">" +
                        page.getTitle() +
                        "</a>";
                    details.append(GT._("The error hasn''t been detected in page {0}.", pageLink));
                    Boolean errorDetected = checkWiki.isErrorDetected(page, errorNumber);
                    if (errorDetected != null) {
                      details.append(" ");
                      if (Boolean.TRUE.equals(errorDetected)) {
                        details.append(GT._("It's still being detected by CheckWiki."));
                      } else {
                        details.append(GT._("It's not detected either by CheckWiki."));
                        unnecessaryPages.add(page);
                      }
                    }
                    details.append("</li>");
                  }
                }
              }
            }

            // Take results into account
            if (details.length() > 0) {

              // Establish a report
              String pageLink = String.valueOf(errorNumber);
              String whiteListPageName = cwConfig.getWhiteListPageName();
              if (whiteListPageName != null) {
                pageLink =
                    "<a href=\"" +
                    wiki.getSettings().getURL(cwConfig.getWhiteListPageName(), false, true) +
                    "\">" +
                    String.valueOf(errorNumber) +
                    "</a>";
              }
              result.append(GT._(
                  "The following problems were detected on the whitelist for error {0}:",
                  pageLink));
              result.append("<ul>");
              result.append(details.toString());
              result.append("</ul>");

              // Update white list
              String comment = wiki.getConfiguration().getString(WPCConfigurationString.CW_WHITELISTE_COMMENT);
              if (!unnecessaryPages.isEmpty() &&
                  (whiteListPageName != null) &&
                  (comment != null)) {
                Page whiteListPage = DataManager.getPage(wiki, whiteListPageName, null, null, null);
                api.retrieveContents(wiki, Collections.singletonList(whiteListPage), false, false);
                String initialContents = whiteListPage.getContents();
                String contents = initialContents;
                for (Page page : unnecessaryPages) {
                  PageAnalysis analysis = whiteListPage.getAnalysis(contents, false);
                  List<PageElementInternalLink> links = analysis.getInternalLinks();
                  for (int linkNumber = links.size(); linkNumber > 0; linkNumber--) {
                    PageElementInternalLink link = links.get(linkNumber - 1);
                    if (Page.areSameTitle(page.getTitle(), link.getLink())) {
                      int lineBegin = link.getBeginIndex();
                      while ((lineBegin > 0) && (contents.charAt(lineBegin) != '\n')) {
                        lineBegin--;
                      }
                      int lineEnd = link.getEndIndex();
                      while ((lineEnd < contents.length()) && (contents.charAt(lineEnd) != '\n')) {
                        lineEnd++;
                      }
                      boolean lineOk = true;
                      for (PageElementInternalLink tmpLink : links) {
                        if ((tmpLink != link) &&
                            (tmpLink.getBeginIndex() < lineEnd) &&
                            (tmpLink.getEndIndex() > lineBegin)) {
                          lineOk = false;
                        }
                      }
                      if (lineOk) {
                        contents = contents.substring(0, lineBegin) + contents.substring(lineEnd);
                      }
                    }
                  }
                }
                if (!contents.equals(initialContents)) {
                  api.updatePage(wiki, whiteListPage, contents, comment, true, false);
                }
              }
            }
          }
        }
      }
      return result.toString();
    } catch (APIException e) {
      return e;
    }
  }

  /**
   * Called on the event dispatching thread (not on the worker thread)
   * after the <code>construct</code> method has returned.
   * 
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    if (getWindow() != null) {
      InformationWindow.createInformationWindow(
          GT._("Whitelists"), get().toString(), true, getWikipedia());
    }
  }

}
