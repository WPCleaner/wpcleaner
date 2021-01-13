/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.awt.Component;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.api.data.AutomaticFormatter;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.execution.AllLinksToPageCallable;
import org.wikipediacleaner.api.execution.ContentsCallable;
import org.wikipediacleaner.api.execution.DisambiguationStatusCallable;
import org.wikipediacleaner.api.execution.EmbeddedInCallable;
import org.wikipediacleaner.api.execution.ExpandTemplatesCallable;
import org.wikipediacleaner.api.execution.LinksWRCallable;
import org.wikipediacleaner.api.execution.ParseTextCallable;
import org.wikipediacleaner.api.execution.TemplatesCallable;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningTools;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Centralization of access to MediaWiki.
 */
public class MediaWiki extends MediaWikiController {

  /**
   * @param listener Listener to MediaWiki events.
   * @return Access to MediaWiki.
   */
  static public MediaWiki getMediaWikiAccess(MediaWikiListener listener) {
    MediaWiki mw = new MediaWiki(listener);
    return mw;
  }

  /**
   * @param listener Listener.
   */
  private MediaWiki(MediaWikiListener listener) {
    super(listener);
  }

  /**
   * Block until all tasks are finished. 
   * 
   * @param block True if method should block until tasks are finished.
   * @throws APIException Exception thrown by the API.
   */
  public void block(boolean block) throws APIException {
    if (block) {
      while (hasRemainingTask() && !shouldStop()) {
        getNextResult();
      }
    }
    if (shouldStop()) {
      stopRemainingTasks();
    }
  }

  /**
   * Retrieve page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param block Flag indicating if the call should block until completed.
   * @param returnPage Flag indicating if the page should be returned once task is finished.
   * @param usePageId True if page identifiers should be used.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @param doAnalysis True if page analysis should be done.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveContents(
      EnumWikipedia wikipedia, Page page,
      boolean block, boolean returnPage,
      boolean usePageId, boolean withRedirects,
      boolean doAnalysis) throws APIException {
    if (page == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    addTask(new ContentsCallable(
        wikipedia, this, api,
        page, returnPage ? page : null,
        usePageId, withRedirects, null,
        doAnalysis));
    block(block);
  }

  /**
   * Retrieve page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param pages Pages.
   * @param block Flag indicating if the call should block until completed.
   * @param usePageId True if page identifiers should be used.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @param doAnalysis True if page analysis should be done.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveContents(
      EnumWikipedia wikipedia, Collection<Page> pages,
      boolean block, boolean usePageId, boolean withRedirects,
      boolean doAnalysis) throws APIException {
    if (pages == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    for (Page page : pages) {
      addTask(new ContentsCallable(
          wikipedia, this, api,
          page, null,
          usePageId, withRedirects, null,
          doAnalysis));
    }
    block(block);
  }

  /**
   * Retrieve page section contents.
   * 
   * @param wikipedia Wikipedia.
   * @param pages Pages.
   * @param section Section.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveSectionContents(
      EnumWikipedia wikipedia, Collection<Page> pages,
      int section, boolean block) throws APIException {
    if (pages == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    for (Page page : pages) {
      addTask(new ContentsCallable(
          wikipedia, this, api,
          page, null,
          false, false, Integer.valueOf(section),
          false));
    }
    block(block);
  }

  /**
   * Replace text in a list of pages.
   * 
   * @param pages List of pages.
   * @param replacements List of text replacements
   *        Key: Additional comments used for the modification.
   *        Value: Text replacements.
   * @param wiki Wiki.
   * @param comment Comment used for the modification.
   * @param report (Out) Report of changes made.
   * @param automaticCW Lit of CW fixing that should be done.
   * @param forceCW List of CW fixing that should be done even if no automatic replacement was done.
   * @param save True if modification should be saved.
   * @param updateDabWarning True to update disambiguation warning.
   * @param minor True if the modification should be tagged as minor.
   * @param pauseAfterEachEdit True to pause after each edit.
   * @param botFix True to apply bot fixes.
   * @param parent Parent window.
   * @return Count of modified pages.
   * @throws APIException Exception thrown by the API.
   */
  public int replaceText(
      Page[] pages, Map<String, List<AutomaticFixing>> replacements,
      EnumWikipedia wiki, String comment,
      ModificationReport report,
      Collection<CheckErrorAlgorithm> automaticCW, Collection<CheckErrorAlgorithm> forceCW,
      boolean save, boolean updateDabWarning, boolean minor,
      boolean pauseAfterEachEdit, boolean botFix, Component parent) throws APIException {
    if ((pages == null) || (replacements == null) || (replacements.size() == 0)) {
      return 0;
    }

    // Initialize page loading
    Configuration config = Configuration.getConfiguration();
    int nThreads = Math.max(
        config.getInt(null, ConfigurationValueInteger.INTERROG_THREAD), 1);
    int currentPage = 0;
    while ((currentPage < nThreads) && (currentPage < pages.length)) {
      retrieveContents(wiki, pages[currentPage], false, true, false, true, false); // TODO: withRedirects=false ?
      pages[currentPage] = null; // To release memory
      currentPage++;
    }

    // Analyze pages
    UpdateDabWarningTools dabWarnings = new UpdateDabWarningTools(wiki, null, false, false);
    int count = 0;
    final API api = APIFactory.getAPI();
    StringBuilder details = new StringBuilder();
    StringBuilder fullComment = new StringBuilder();
    ModificationReport.Modification modification = null;
    boolean stopRequested = false;
    while (hasRemainingTask() && !shouldStop() && !stopRequested) {
      Object result = getNextResult();
      if (currentPage < pages.length) {
        retrieveContents(wiki, pages[currentPage], false, true, false, true, false); // TODO: withRedirects=false ?
        pages[currentPage] = null; // To release memory
        currentPage++;
      }
      if ((result != null) && (result instanceof Page)) {
        List<String> replacementsDone = new ArrayList<>();
        Page page = (Page) result;
        String oldContents = page.getContents();
        if (oldContents != null) {
          String newContents = oldContents;
          details.setLength(0);
          fullComment.setLength(0);
          if (report != null) {
            modification = new ModificationReport.Modification(page.getTitle());
          }

          // Apply automatic fixing
          for (Entry<String, List<AutomaticFixing>> replacement : replacements.entrySet()) {
            replacementsDone.clear();
            String tmpContents = AutomaticFixing.apply(replacement.getValue(), newContents, replacementsDone);
            if (!newContents.equals(tmpContents)) {
              newContents = tmpContents;

              // Update description
              if (modification != null) {
                for (String replacementDone : replacementsDone) {
                  modification.addModification(replacementDone);
                }
              }

              // Memorize replacement
              if ((replacement.getKey() != null) && (replacement.getKey().length() > 0)) {
                if (details.length() > 0) {
                  details.append(", ");
                }
                details.append(replacement.getKey());
              }
            }
          }
          fullComment.append(wiki.createUpdatePageComment(comment, details.toString()));

          // Apply automatic CW fixing if needed
          if (automaticCW != null) {

            // Apply fixing
            List<AlgorithmError.Progress> usedAlgorithms = new ArrayList<>();
            String tmpContents = AutomaticFormatter.tidyArticle(
                page, newContents, automaticCW, botFix, usedAlgorithms);

            // Decide if modifications should be kept
            boolean shouldKeep = (!oldContents.equals(newContents));
            if (forceCW != null) {
              for (AlgorithmError.Progress progress : usedAlgorithms) {
                if (forceCW.contains(progress.algorithm)) {
                  shouldKeep = true;
                }
              }
            }

            // Keep modifications
            if (shouldKeep) {
              newContents = tmpContents;
              if (!usedAlgorithms.isEmpty()) {
                fullComment.append(" / ");
                fullComment.append(wiki.getCWConfiguration().getComment(usedAlgorithms));
                if (modification != null) {
                  for (AlgorithmError.Progress progress : usedAlgorithms) {
                    CheckErrorAlgorithm algorithm = progress.algorithm;
                    modification.addModification(algorithm.getShortDescriptionReplaced());
                  }
                }
              }
            }
          }

          // Page contents has been modified
          if (!oldContents.equals(newContents)) {
            if (report != null) {
              report.addModification(modification);
            }

            // Save page
            setText(GT._T("Updating page {0}", page.getTitle()));
            count++;
            if (save && !stopRequested) {
              try {
                api.updatePage(
                    wiki, page, newContents, fullComment.toString(),
                    true, minor, false, false);
                if (updateDabWarning) {
                  List<Page> tmpList = new ArrayList<>(1);
                  tmpList.add(page);
                  dabWarnings.updateWarning(tmpList, null, null, null);
                }
                if (pauseAfterEachEdit) {
                  int answer = Utilities.displayYesNoAllWarning(
                      parent,
                      GT._T("The page {0} has been modified.", page.getTitle()) + "\n" +
                      GT._T("Do you want to continue?"));
                  switch (answer) {
                  case JOptionPane.YES_OPTION:
                    break;
                  case Utilities.YES_ALL_OPTION:
                    pauseAfterEachEdit = false;
                    break;
                  default:
                    stopRequested = true;
                  }
                }
              } catch (APIException e) {
                EnumQueryResult error = e.getQueryResult();
                if (report != null) {
                  report.addError(new ModificationReport.Error(page.getTitle(), error));
                }
                if (EnumQueryResult.PROTECTED_PAGE.equals(error)) {
                  System.err.println("Page " + page.getTitle() + " is protected.");
                } else {
                  throw e;
                }
              }
            }
          }
        }
      }
    }
    block(true);
    return count;
  }

  /**
   * Expand templates.
   * 
   * @param wikipedia Wikipedia.
   * @param title Title of the page.
   * @param text Text of the page.
   * @return Expanded text.
   * @throws APIException Exception thrown by the API.
   */
  public String expandTemplates(EnumWikipedia wikipedia, String title, String text) throws APIException {
    if (text == null) {
      return null;
    }
    final API api = APIFactory.getAPI();
    addTask(new ExpandTemplatesCallable(wikipedia, this, api, title, text));
    while (hasRemainingTask() && !shouldStop()) {
      Object result = getNextResult();
      if (result != null) {
        return result.toString();
      }
    }
    block(true);
    return null;
  }

  /**
   * Parse complete text.
   * 
   * @param wikipedia Wikipedia.
   * @param title Title of the page.
   * @param text Text of the page.
   * @return Parsed text.
   * @throws APIException Exception thrown by the API.
   */
  public String parseText(
      EnumWikipedia wikipedia,
      String title, String text) throws APIException {
    if (text == null) {
      return null;
    }
    final API api = APIFactory.getAPI();
    addTask(new ParseTextCallable(wikipedia, this, api, title, text));
    while (hasRemainingTask() && !shouldStop()) {
      Object result = getNextResult();
      if (result != null) {
        return result.toString();
      }
    }
    block(true);
    return null;
  }

  /**
   * Retrieve similar pages of a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveSimilarPages(
      EnumWikipedia wikipedia,
      Page page) throws APIException {
    if (page == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    api.retrieveSimilarPages(wikipedia, page, true);
  }

  /**
   * Retrieve all links (with redirects) of a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param namespace If set, retrieve only links in this namespace.
   * @param knownPages Already known pages.
   * @param disambigNeeded True if disambiguation information is needed.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveAllLinks(
      EnumWikipedia wikipedia,
      Page page, Integer namespace,
      List<Page> knownPages,
      boolean disambigNeeded,
      boolean block) throws APIException {
    if (page == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    addTask(new LinksWRCallable(wikipedia, this, api, page, namespace, knownPages, disambigNeeded));
    block(block);
  }

  /**
   * Retrieve all templates of a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveAllTemplates(
      EnumWikipedia wikipedia,
      Page page,
      boolean block) throws APIException {
    if (page == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    addTask(new TemplatesCallable(wikipedia, this, api, page));
    block(block);
  }

  /**
   * Retrieve all links to a page (with redirects).
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveAllLinksToPage(
      EnumWikipedia wikipedia,
      Page page, boolean block) throws APIException {
    if (page == null) {
      return;
    }
    retrieveAllLinksToPages(wikipedia, Collections.singleton(page), block);
  }

  /**
   * Retrieve all links to a list of pages (with redirects).
   * 
   * @param wikipedia Wikipedia.
   * @param pageList List of pages.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveAllLinksToPages(
      EnumWikipedia wikipedia,
      Collection<Page> pageList, boolean block) throws APIException {
    if ((pageList == null) || (pageList.size() == 0)) {
      return;
    }
    final API api = APIFactory.getAPI();
    for (final Page page : pageList) {
      addTask(new AllLinksToPageCallable(wikipedia, this, api, page));
    }
    block(block);
  }

  /**
   * Retrieve all pages it is embedded in of a list of pages.
   * 
   * @param wikipedia Wikipedia.
   * @param pageList List of pages.
   * @param namespaces List of name spaces to look into.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException Exception thrown by the API.
   * @return Pages.
   */
  @SuppressWarnings("unchecked")
  public List<Page> retrieveAllEmbeddedIn(
      EnumWikipedia wikipedia, List<Page> pageList,
      List<Integer> namespaces,
      boolean limit) throws APIException {
    if ((pageList == null) || (pageList.size() == 0)) {
      return null;
    }
    final API api = APIFactory.getAPI();
    for (final Page page : pageList) {
      addTask(new EmbeddedInCallable(wikipedia, this, api, page, namespaces, limit));
    }
    List<Page> resultList = new ArrayList<>();
    while (hasRemainingTask() && !shouldStop()) {
      Object result = getNextResult();
      if (result instanceof List<?>) {
        List<Page> pageResult = (List<Page>) result;
        for (Page page : pageResult) {
          resultList.add(page);
        }
      }
    }
    Collections.sort(resultList);
    Iterator<Page> itPage = resultList.iterator();
    Page previousPage = null;
    while (itPage.hasNext()) {
      Page page = itPage.next();
      if ((previousPage != null) &&
          (Page.areSameTitle(previousPage.getTitle(), page.getTitle()))) {
        itPage.remove();
      } else {
        previousPage = page;
      }
    }
    return resultList;
  }

  /**
   * Retrieve disambiguation information for a list of pages.
   * 
   * @param wikipedia Wikipedia.
   * @param pageList List of page.
   * @param knownPages Already known pages.
   * @param disambiguations Flag indicating if possible disambiguations should be retrieved.
   * @param forceApiCall True if API call should be forced even if the list of disambiguation pages is loaded.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveDisambiguationInformation(
      EnumWikipedia wikipedia,
      List<Page> pageList, List<Page> knownPages,
      boolean disambiguations, boolean forceApiCall, boolean block)
          throws APIException {
    if ((pageList == null) || (pageList.isEmpty())) {
      return;
    }
    final API api = APIFactory.getAPI();

    // Retrieving disambiguation status
    final int maxPages = api.getMaxPagesPerQuery();
    List<Page> filteredList = pageList;
    if (knownPages != null) {
      filteredList = new ArrayList<>(pageList);
      filteredList.removeAll(knownPages);
    }
    if (filteredList.size() <= maxPages) {
      addTask(new DisambiguationStatusCallable(wikipedia, this, api, filteredList, forceApiCall));
    } else {
      int index = 0;
      while (index < filteredList.size()) {
        List<Page> tmpList = new ArrayList<>(api.getMaxPagesPerQuery());
        for (int i = 0; (i < maxPages) && (index < filteredList.size()); i++, index++) {
          tmpList.add(filteredList.get(index));
        }
        addTask(new DisambiguationStatusCallable(wikipedia, this, api, tmpList, forceApiCall));
      }
    }
    block(true);

    // Retrieving possible disambiguations
    if (disambiguations) {
      for (Page p : pageList) {
        Iterator<Page> iter = p.getRedirects().getIteratorWithPage();
        while (iter.hasNext()) {
          p = iter.next();
          if ((Boolean.TRUE.equals(p.isDisambiguationPage())) &&
              (!p.getRedirects().isRedirect())) {
            List<Page> links = p.getLinks();
            if ((links == null) || (links.size() == 0)) {
              addTask(new LinksWRCallable(wikipedia, this, api, p, null, null, false));
            }
          }
        }
      }
    }
    block(block);
  }
}
