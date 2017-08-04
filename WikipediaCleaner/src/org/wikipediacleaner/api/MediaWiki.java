/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
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
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningTools;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @param description (Out) description of changes made.
   * @param automaticCW True if automatic Check Wiki fixing should be done also.
   * @param forceCW List of Check Wiki fixing that should be done even if no automatic replacement was done.
   * @param save True if modification should be saved.
   * @param minor True if the modification should be tagged as minor.
   * @return Count of modified pages.
   * @throws APIException
   */
  public int replaceText(
      Page[] pages, Map<String, List<AutomaticFixing>> replacements,
      EnumWikipedia wiki, String comment,
      StringBuilder description,
      boolean automaticCW, Collection<CheckErrorAlgorithm> forceCW, boolean save,
      boolean updateDabWarning, boolean minor) throws APIException {
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
    boolean secured = config.getBoolean(null, ConfigurationValueBoolean.SECURE_URL);
    UpdateDabWarningTools dabWarnings = new UpdateDabWarningTools(wiki, null, false, false);
    int count = 0;
    final API api = APIFactory.getAPI();
    StringBuilder details = new StringBuilder();
    StringBuilder fullComment = new StringBuilder();
    StringBuilder tmpDescription = (description != null) ? new StringBuilder() : null;
    while (hasRemainingTask() && !shouldStop()) {
      Object result = getNextResult();
      if (currentPage < pages.length) {
        retrieveContents(wiki, pages[currentPage], false, true, false, true, false); // TODO: withRedirects=false ?
        pages[currentPage] = null; // To release memory
        currentPage++;
      }
      if ((result != null) && (result instanceof Page)) {
        List<String> replacementsDone = new ArrayList<String>();
        Page page = (Page) result;
        String oldContents = page.getContents();
        if (oldContents != null) {
          String newContents = oldContents;
          details.setLength(0);
          fullComment.setLength(0);
          if (tmpDescription != null) {
            tmpDescription.setLength(0);
            String title =
                "<a href=\"" + wiki.getSettings().getURL(page.getTitle(), false, secured) + "\">" +
                page.getTitle() + "</a>";
            tmpDescription.append(GT._("Page {0}:", title));
            tmpDescription.append("\n");
            tmpDescription.append("<ul>\n");
          }

          // Apply automatic fixing
          for (Entry<String, List<AutomaticFixing>> replacement : replacements.entrySet()) {
            replacementsDone.clear();
            String tmpContents = AutomaticFixing.apply(replacement.getValue(), newContents, replacementsDone);
            if (!newContents.equals(tmpContents)) {
              newContents = tmpContents;

              // Update description
              if (tmpDescription != null) {
                for (String replacementDone : replacementsDone) {
                  tmpDescription.append("<li>");
                  tmpDescription.append(replacementDone.replaceAll("\\&", "&amp;").replaceAll("\\<", "&lt;"));
                  tmpDescription.append("</li>\n");
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

          // Apply automatic Check Wiki fixing if needed
          if (automaticCW) {

            // Decide if Check Wiki fixing should be applied
            boolean shouldApply = false;
            if (!oldContents.equals(newContents)) {
              shouldApply = true;
            } else if ((forceCW != null) && !forceCW.isEmpty()) {
              shouldApply = true;
            }

            // Apply Check Wiki fixing
            if (shouldApply) {
              String tmpContents = newContents;
              List<CheckErrorAlgorithm> algorithms = CheckErrorAlgorithms.getAlgorithms(wiki);
              List<CheckError.Progress> usedAlgorithms = new ArrayList<>();
              newContents = AutomaticFormatter.tidyArticle(
                  page, newContents, algorithms, false, usedAlgorithms);

              // Decide if modifications should be kept
              boolean shouldKeep = (!oldContents.equals(tmpContents));
              if (forceCW != null) {
                for (CheckError.Progress progress : usedAlgorithms) {
                  if (forceCW.contains(progress.algorithm)) {
                    shouldKeep = true;
                  }
                }
              }

              // Keep modifications
              if (shouldKeep && !usedAlgorithms.isEmpty()) {
                fullComment.append(" / ");
                fullComment.append(wiki.getCWConfiguration().getComment(usedAlgorithms));
                if (tmpDescription != null) {
                  for (CheckError.Progress progress : usedAlgorithms) {
                    CheckErrorAlgorithm algorithm = progress.algorithm;
                    tmpDescription.append("<li>");
                    tmpDescription.append(algorithm.getShortDescriptionReplaced());
                    tmpDescription.append("</li>\n");
                  }
                }
              } else if (oldContents.equals(tmpContents)) {
                // If no automatic modifications done before CW, don't keep the modifications for general tidiness
                newContents = tmpContents;
              }
            }
          }

          // Page contents has been modified
          if (!oldContents.equals(newContents)) {
            if (tmpDescription != null) {
              tmpDescription.append("</ul>\n");
              if (description != null) {
                description.append(tmpDescription);
              }
            }

            // Save page
            setText(GT._("Updating page {0}", page.getTitle()));
            count++;
            if (save) {
              try {
                api.updatePage(
                    wiki, page, newContents, fullComment.toString(), minor, false, false);
                if (updateDabWarning) {
                  List<Page> tmpList = new ArrayList<>(1);
                  tmpList.add(page);
                  dabWarnings.updateWarning(tmpList, null, null, null);
                }
              } catch (APIException e) {
                EnumQueryResult error = e.getQueryResult();
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
   * @throws APIException
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
    List<Page> resultList = new ArrayList<Page>();
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
   * @throws APIException
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
      filteredList = new ArrayList<Page>(pageList);
      filteredList.removeAll(knownPages);
    }
    if (filteredList.size() <= maxPages) {
      addTask(new DisambiguationStatusCallable(wikipedia, this, api, filteredList, forceApiCall));
    } else {
      int index = 0;
      while (index < filteredList.size()) {
        List<Page> tmpList = new ArrayList<Page>(api.getMaxPagesPerQuery());
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
        Iterator<Page> iter = p.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          p = iter.next();
          if ((Boolean.TRUE.equals(p.isDisambiguationPage())) &&
              (!p.isRedirect())) {
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
