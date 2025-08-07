/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
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
    List<PageResult> unnecessaryPages = new ArrayList<>();
    boolean hasUnnecessaryPages = false;
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    for (Page page : pages) {
      if (Boolean.FALSE.equals(page.isExisting())) {
        unnecessaryPages.add(PageResult.ofMissingPage(page));
        hasUnnecessaryPages = true;
      } else {
        CheckErrorPage errorPage = AlgorithmError.analyzeError(
            algorithm, page.getAnalysis(page.getContents(), true));
        if (errorPage == null || !errorPage.getErrorFound()) {
          if (algorithm.getErrorNumber() < CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
            Boolean errorDetected = checkWiki.isErrorDetected(page, errorNumber);
            if (errorDetected != null) {
              unnecessaryPages.add(PageResult.ofExistingPageWithCheckWiki(page, errorDetected));
              hasUnnecessaryPages |= !errorDetected;
            }
          } else {
            unnecessaryPages.add(PageResult.ofExistingPageWithoutCheckWiki(page));
            hasUnnecessaryPages = true;
          }
        }
      }
    }
    if (!hasUnnecessaryPages) {
      return;
    }

    // Update white list
    String comment = wiki.getConfiguration().getString(WPCConfigurationString.CW_WHITELISTE_COMMENT);
    String whiteListPageName = cwConfig.getWhiteListPageName();
    Set<PageResult> removed = new HashSet<>();
    if (whiteListPageName != null && comment != null) {
      API api = APIFactory.getAPI();
      Page whiteListPage = DataManager.createSimplePage(wiki, whiteListPageName, null, null, null);
      api.retrieveContents(wiki, Collections.singletonList(whiteListPage), false, false);
      String initialContents = whiteListPage.getContents();
      String contents = initialContents;
      PageAnalysis analysis = whiteListPage.getAnalysis(contents, false);
      List<PageElementInternalLink> links = analysis.getInternalLinks();
      for (int linkNumber = links.size(); linkNumber > 0; linkNumber--) {
        PageElementInternalLink link = links.get(linkNumber - 1);
        PageResult pageResult = unnecessaryPages.stream()
            .filter(tmp -> Page.areSameTitle(tmp.page().getTitle(), link.getLink()))
            .filter(PageResult::shouldBeRemoved)
            .findFirst().orElse(null);
        if (pageResult != null && link.getNamespace(wiki) != Namespace.SPECIAL) {
          int lineBegin = ContentsUtil.moveIndexBackwardWhileNotFound(contents, link.getBeginIndex(), "\n") + 1;
          int lineEnd = ContentsUtil.moveIndexForwardWhileNotFound(contents, link.getEndIndex(), "\n");
          boolean lineOk = true;
          for (PageElementInternalLink tmpLink : links) {
            boolean inSameLine = (tmpLink != link) && tmpLink.overlap(lineBegin, lineEnd);
            boolean isSpecial = tmpLink.getNamespace(wiki) == Namespace.SPECIAL;
            if (inSameLine && !isSpecial) {
              lineOk = false;
            }
          }
          if (lineOk) {
            contents = "%s<!-- %s -->%s".formatted(
                contents.substring(0, lineBegin),
                contents.substring(lineBegin, lineEnd),
                contents.substring(lineEnd));
            removed.add(pageResult);
          }
        }
      }
      if (!contents.equals(initialContents)) {
        api.updatePage(
            wiki, whiteListPage, contents, comment,
            true, true, true, false);
      }
    }

    // Establish a report
    String pageLink = String.valueOf(errorNumber);
    if (whiteListPageName != null) {
      pageLink = CompleteTagBuilder.from(HtmlTagType.A, String.valueOf(errorNumber))
          .addAttribute("href", wiki.getSettings().getURL(cwConfig.getWhiteListPageName(), false, true)).toString();
    }
    result.append(GT._T("Regarding whitelist for error {0}:", pageLink));
    result.append(HtmlTagType.UL.getOpenTag());
    for (PageResult page : unnecessaryPages) {
      final String title = page.page().getTitle();
      result.append(HtmlTagType.LI.getOpenTag());
      pageLink = CompleteTagBuilder.from(HtmlTagType.A, title)
          .addAttribute("href", wiki.getSettings().getURL(title, false, true)).toString();
      if (page.shouldBeRemoved()) {
        result.append(GT._T("Page {0} should be removed from the whitelist.", pageLink));
      } else {
        result.append(GT._T("Page {0} should be kept in the whitelist.", pageLink));
      }
      result.append(" ");
      if (!page.exists()) {
        result.append(GT._T("It doesn't exist on Wikipedia."));
      } else if (!page.checkWiki()) {
        result.append(GT._T("The error hasn't been detected by WPCleaner, and CheckWiki doesn't handle this error."));
      } else {
        if (page.checkWikiResult()) {
          result.append(GT._T("The error hasn't been detected by WPCleaner but it's still detected by CheckWiki."));
        } else {
          result.append(GT._T("The error hasn't been detected neither by WPCleaner or CheckWiki."));
        }
      }
      if (page.shouldBeRemoved() && !removed.contains(page)) {
        result.append(" ").append(GT._T("WPCleaner couldn't perform the removal."));
      }
      result.append(HtmlTagType.LI.getCloseTag());
    }
    result.append(HtmlTagType.UL.getCloseTag());
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

  private record PageResult(Page page, boolean exists, boolean checkWiki, boolean checkWikiResult) {
    public static PageResult ofMissingPage(final Page page) {
      return new PageResult(page, false, false, false);
    }

    public static PageResult ofExistingPageWithoutCheckWiki(final Page page) {
      return new PageResult(page, true, false, false);
    }

    public static PageResult ofExistingPageWithCheckWiki(final Page page, final boolean checkWikiResult) {
      return new PageResult(page, true, true, checkWikiResult);
    }

    boolean shouldBeRemoved() {
      return !exists || !checkWiki || !checkWikiResult;
    }
  }
}
