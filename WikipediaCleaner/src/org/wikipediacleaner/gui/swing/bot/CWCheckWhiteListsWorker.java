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
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.configuration.CWConfigurationError;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.CompleteTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
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
      StringBuilder result = new StringBuilder();
      List<CheckErrorAlgorithm> algorithms = CheckErrorAlgorithms.getAlgorithms(getWikipedia());
      for (CheckErrorAlgorithm algorithm : algorithms) {
        processAlgorithm(algorithm, result);
      }
      return result.toString();
    } catch (APIException e) {
      return e;
    }
  }

  private void processAlgorithm(CheckErrorAlgorithm algorithm, StringBuilder result) throws APIException {
    EnumWikipedia wiki = getWikipedia();
    int errorNumber = algorithm.getErrorNumber();
    if (!algorithm.isAvailable() || !CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
      return;
    }

    setText(GT._T("Checking whitelist for error {0}", String.valueOf(errorNumber)));
    CWConfigurationError cwConfig = wiki.getCWConfiguration().getErrorConfiguration(errorNumber);
    Set<String> whiteList = cwConfig.getWhiteList();
    if (whiteList == null || whiteList.isEmpty()) {
      return;
    }

    List<Page> unnecessaryPages = new ArrayList<>();
    StringBuilder details = new StringBuilder();

    // Prepare list of pages to check
    List<Page> pages = new ArrayList<>(whiteList.size());
    for (String pageName : whiteList) {
      Page page = DataManager.createSimplePage(wiki, pageName, null, null, null);
      pages.add(page);
    }
    Collections.sort(pages);
    MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
    mw.retrieveContents(wiki, pages, true, false, false, false);

    // Check each page
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    for (Page page : pages) {
      if (Boolean.FALSE.equals(page.isExisting())) {
        details.append(CompleteTagBuilder
            .from(HtmlTagType.LI, GT._T("The page {0} doesn''t exist on Wikipedia", page.getTitle())));
        unnecessaryPages.add(page);
      } else {
        CheckErrorPage errorPage = AlgorithmError.analyzeError(
            algorithm, page.getAnalysis(page.getContents(), true));
        if ((errorPage == null) || (!errorPage.getErrorFound())) {
          details.append(HtmlTagType.LI.getOpenTag());
          String pageLink = CompleteTagBuilder.from(HtmlTagType.A, page.getTitle())
              .addAttribute("href", wiki.getSettings().getURL(page.getTitle(), false, true)).toString();
          details.append(GT._T("The error hasn''t been detected in page {0}.", pageLink));
          Boolean errorDetected = checkWiki.isErrorDetected(page, errorNumber);
          if (errorDetected != null) {
            details.append(" ");
            if (errorDetected) {
              details.append(GT._T("It's still being detected by CheckWiki."));
            } else {
              details.append(GT._T("It's not detected either by CheckWiki."));
              unnecessaryPages.add(page);
            }
          }
          details.append(HtmlTagType.LI.getCloseTag());
        }
      }
    }
    if (details.isEmpty()) {
      return;
    }

    // Establish a report
    String pageLink = String.valueOf(errorNumber);
    String whiteListPageName = cwConfig.getWhiteListPageName();
    if (whiteListPageName != null) {
      pageLink = CompleteTagBuilder.from(HtmlTagType.A, String.valueOf(errorNumber))
          .addAttribute("href", wiki.getSettings().getURL(cwConfig.getWhiteListPageName(), false, true)).toString();
    }
    result.append(GT._T(
        "The following problems were detected on the whitelist for error {0}:",
        pageLink));
    result.append(CompleteTagBuilder.from(HtmlTagType.UL, details.toString()));

    // Update white list
    String comment = wiki.getConfiguration().getString(WPCConfigurationString.CW_WHITELISTE_COMMENT);
    if (unnecessaryPages.isEmpty() || whiteListPageName  == null || comment == null) {
      return;
    }
    API api = APIFactory.getAPI();
    Page whiteListPage = DataManager.createSimplePage(wiki, whiteListPageName, null, null, null);
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
            contents = "%s<!-- %s -->%s".formatted(
                contents.substring(0, lineBegin),
                contents.substring(lineBegin, lineEnd),
                contents.substring(lineEnd));
          }
        }
      }
    }
    if (!contents.equals(initialContents)) {
      api.updatePage(
          wiki, whiteListPage, contents, comment,
          true, true, true, false);
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
          GT._T("Whitelists"), get().toString(), true, getWikipedia());
    }
  }

}
