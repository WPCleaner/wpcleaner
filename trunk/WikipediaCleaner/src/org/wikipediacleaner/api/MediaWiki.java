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

package org.wikipediacleaner.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.execution.BacklinksWRCallable;
import org.wikipediacleaner.api.execution.ContentsCallable;
import org.wikipediacleaner.api.execution.DisambiguationStatusCallable;
import org.wikipediacleaner.api.execution.EmbeddedInCallable;
import org.wikipediacleaner.api.execution.ExpandTemplatesCallable;
import org.wikipediacleaner.api.execution.LinksWRCallable;
import org.wikipediacleaner.api.execution.ParseTextCallable;
import org.wikipediacleaner.i18n.GT;


/**
 * Centralisation of access to MediaWiki.
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
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   */
  public void retrieveContents(
      EnumWikipedia wikipedia, Page page,
      boolean block, boolean returnPage, boolean withRedirects) throws APIException {
    if (page == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    addTask(new ContentsCallable(
        wikipedia, this, api,
        page, returnPage ? page : null, withRedirects));
    block(block);
  }

  /**
   * Retrieve page contents.
   * 
   * @param wikipedia Wikipedia.
   * @param pages Pages.
   * @param block Flag indicating if the call should block until completed.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   */
  public void retrieveContents(
      EnumWikipedia wikipedia, Collection<Page> pages,
      boolean block, boolean withRedirects) throws APIException {
    if (pages == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    for (Page page : pages) {
      addTask(new ContentsCallable(
          wikipedia, this, api,
          page, null, withRedirects));
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
   * @param wikipedia Wikipedia.
   * @param comment Comment used for the modification.
   * @param description Out: description of changes made.
   * @throws APIException
   */
  public int replaceText(
      Page[] pages, Map<String, Properties> replacements,
      EnumWikipedia wikipedia, String comment,
      StringBuilder description) throws APIException {
    if ((pages == null) || (replacements == null) || (replacements.size() == 0)) {
      return 0;
    }
    for (Page page : pages) {
      retrieveContents(wikipedia, page, false, true, true); // TODO: withRedirects=false ?
    }
    int count = 0;
    final API api = APIFactory.getAPI();
    StringBuilder details = new StringBuilder();
    while (hasRemainingTask() && !shouldStop()) {
      Object result = getNextResult();
      if ((result != null) && (result instanceof Page)) {
        boolean changed = false;
        Page page = (Page) result;
        String oldContents = page.getContents();
        if (oldContents != null) {
          String newContents = oldContents;
          details.setLength(0);
          for (Entry<String, Properties> replacement : replacements.entrySet()) {
            boolean replacementUsed = false;
            for (Entry<Object, Object> replacementValue : replacement.getValue().entrySet()) {
              String from = replacementValue.getKey().toString();
              String to = replacementValue.getValue().toString();
              String tmpContents = newContents;
              if (to.indexOf('$') >= 0) {
                // Replacement: "$" -> "\$" to avoid interpretation by replaceAll
                to = to.replaceAll(Pattern.quote("$"), "\\\\\\$");
              }
              newContents = tmpContents.replaceAll(Pattern.quote(from), to);
              if (!newContents.equals(tmpContents)) {
                if (description != null) {
                  if (!changed) {
                    String title =
                      "<a href=\"" + wikipedia.getWikiURL(page.getTitle(), false) + "\">" +
                      page.getTitle() + "</a>";
                    description.append(GT._("Page {0}:", title));
                    description.append("\n");
                    description.append("<ul>\n");
                    changed = true;
                  }
                  description.append("<li>");
                  description.append(from);
                  description.append(" => ");
                  description.append(to);
                  description.append("</li>\n");
                }
                if (!replacementUsed) {
                  replacementUsed = true;
                  if (details.length() > 0) {
                    details.append(", ");
                  }
                  details.append(replacement.getKey());
                }
              }
            }
          }
          if ((description != null) && (changed)) {
            description.append("</ul>\n");
          }
          if (!oldContents.equals(newContents)) {
            setText(GT._("Updating page {0}", page.getTitle()));
            count++;
            try {
              api.updatePage(
                  wikipedia, page, newContents,
                  wikipedia.createUpdatePageComment(comment, details.toString()),
                  false);
            } catch (APIException e) {
              if (APIException.ERROR_BAD_TOKEN.equals(e.getErrorCode())) {
                api.retrieveContents(wikipedia, page, false);
                api.updatePage(
                    wikipedia, page, newContents,
                    wikipedia.createUpdatePageComment(comment, details.toString()),
                    false);
              } else {
                throw e;
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
   * Retrieve all links (with redirects) of a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param namespace If set, retrieve only links in this namespace.
   * @param knownPages Already known pages.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException
   */
  public void retrieveAllLinks(
      EnumWikipedia wikipedia,
      Page page, Integer namespace,
      List<Page> knownPages,
      boolean block) throws APIException {
    if (page == null) {
      return;
    }
    final API api = APIFactory.getAPI();
    addTask(new LinksWRCallable(wikipedia, this, api, page, namespace, knownPages));
    block(block);
  }

  /**
   * Retrieve all backlinks (with redirects) of a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException
   */
  public void retrieveAllBacklinks(
      EnumWikipedia wikipedia,
      Page page, boolean block) throws APIException {
    if (page == null) {
      return;
    }
    retrieveAllBacklinks(wikipedia, new Page[] { page }, block);
  }

  /**
   * Retrieve all backlinks (with redirects) of a list of pages.
   * 
   * @param wikipedia Wikipedia.
   * @param pageList List of pages.
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException
   */
  public void retrieveAllBacklinks(
      EnumWikipedia wikipedia,
      Page[] pageList, boolean block) throws APIException {
    if ((pageList == null) || (pageList.length == 0)) {
      return;
    }
    final API api = APIFactory.getAPI();
    for (final Page page : pageList) {
      addTask(new BacklinksWRCallable(wikipedia, this, api, page));
    }
    while (hasRemainingTask() && !shouldStop()) {
      Object result = getNextResult();
      if (result != null) {
        if (result instanceof Page) {
          final Page page = (Page) result;
          List<Page> backlinks = page.getBackLinks();
          Iterator<Page> iter1 = backlinks.iterator();
          while (iter1.hasNext()) {
            final Page p = iter1.next();
            if (!Page.areSameTitle(page.getTitle(), p.getTitle())) {
              Iterator<Page> iter = p.getRedirectIteratorWithPage();
              while (iter.hasNext()) {
                Page tmp = iter.next();
                if (Page.areSameTitle(page.getTitle(), tmp.getTitle())) {
                  addTask(new BacklinksWRCallable(wikipedia, this, api, p));
                }
              }
            }
          }
        }
      }
    }
    block(block);
  }

  /**
   * Retrieve all pages it is embedded in of a list of pages.
   * 
   * @param wikipedia Wikipedia.
   * @param pageList List of pages.
   * @throws APIException
   */
  @SuppressWarnings("unchecked")
  public List<Page> retrieveAllEmbeddedIn(
      EnumWikipedia wikipedia, List<Page> pageList) throws APIException {
    if ((pageList == null) || (pageList.size() == 0)) {
      return null;
    }
    final API api = APIFactory.getAPI();
    for (final Page page : pageList) {
      addTask(new EmbeddedInCallable(wikipedia, this, api, page));
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
   * @param block Flag indicating if the call should block until completed.
   * @throws APIException
   */
  public void retrieveDisambiguationInformation(
      EnumWikipedia wikipedia,
      List<Page> pageList, List<Page> knownPages,
      boolean disambiguations, boolean block) throws APIException {
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
      addTask(new DisambiguationStatusCallable(wikipedia, this, api, filteredList));
    } else {
      int index = 0;
      while (index < filteredList.size()) {
        List<Page> tmpList = new ArrayList<Page>(api.getMaxPagesPerQuery());
        for (int i = 0; (i < maxPages) && (index < filteredList.size()); i++, index++) {
          tmpList.add(filteredList.get(index));
        }
        addTask(new DisambiguationStatusCallable(wikipedia, this, api, tmpList));
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
              addTask(new LinksWRCallable(wikipedia, this, api, p, null, null));
            }
          }
        }
      }
    }
    block(block);
  }
}
