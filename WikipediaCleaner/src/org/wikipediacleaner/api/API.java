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

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.api.data.RecentChange;


/**
 * MediaWiki API.
 */
public interface API {

  /**
   * @return Maximum number of pages per query.
   */
  public int getMaxPagesPerQuery();

  /**
   * Load Wikipedia configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param userName User name.
   */
  public void loadConfiguration(
      EnumWikipedia wikipedia,
      String userName) throws APIException;

  /**
   * Retrieves the contents of a section in a <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param section Section number.
   * @throws APIException
   */
  public void retrieveSectionContents(EnumWikipedia wikipedia, Page page, int section) throws APIException;

  /**
   * Update a page on Wikipedia.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updatePage(
      EnumWikipedia wikipedia, Page page,
      String newContents, String comment,
      boolean forceWatch) throws APIException;

  /**
   * Add a new section in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param title Title of the new section.
   * @param contents Contents.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult addNewSection(
      EnumWikipedia wikipedia,
      Page page, String title, String contents, boolean forceWatch) throws APIException;


  /**
   * Update a section in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param title Title of the new section.
   * @param section Section. 
   * @param contents Contents.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updateSection(
      EnumWikipedia wikipedia,
      Page page, String title, int section,
      String contents, boolean forceWatch) throws APIException;

  /**
   * Retrieves the links of <code>page</code> and initialize redirect status.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param namespace If set, retrieve only links in this namespace.
   * @param knownPages Already known pages.
   * @throws APIException
   */
  public void retrieveLinksWithRedirects(
      EnumWikipedia wikipedia,
      Page page, Integer namespace,
      List<Page> knownPages) throws APIException;

  /**
   * Retrieves the templates of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @throws APIException
   */
  public void retrieveTemplates(EnumWikipedia wikipedia, Page page) throws APIException;

  /**
   * Initialize the information concerning redirects.
   * 
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(EnumWikipedia wikipedia, List<Page> pages) throws APIException;

  // ==========================================================================
  // API : Authentication
  // ==========================================================================

  /**
   * Login into Wiki.
   * (<code>action=login</code>).
   * 
   * @param wiki Wiki.
   * @param username User name.
   * @param password Password.
   * @param login Flag indicating if login should be done.
   * @return Login status.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Login">API:Login</a>
   */
  public LoginResult login(
      EnumWikipedia wikipedia,
      String username,
      String password,
      boolean login) throws APIException;

  /**
   * Logout.
   * (<code>action=logout</code>).
   * 
   * @see <a href="http://www.mediawiki.org/wiki/API:Logout">API:Logout</a>
   */
  public void logout();

  // ==========================================================================
  // API : Queries / Meta information
  // ==========================================================================

  // ==========================================================================
  // API : Queries / Properties
  // ==========================================================================

  /**
   * Retrieves the contents of <code>page</code>.
   * (<code>action=query</code>, <code>prop=revisions</code>).
   * 
   * @param wiki Wiki.
   * @param pages The pages.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#revisions_.2F_rv">API:Properties#revisions</a>
   */
  public void retrieveContents(
      EnumWikipedia wikipedia,
      Collection<Page> pages, boolean withRedirects) throws APIException;

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * (<code>action=query</code>, <code>prop=categories</code>) or
   * (<code>action=query</code>, <code>prop=templates</code>).
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#categories_.2F_cl">API:Properties#categories</a>
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#templates_.2F_tl">API:Properties#templates</a>
   */
  public void initializeDisambiguationStatus(
      EnumWikipedia wikipedia, List<Page> pages) throws APIException;

  /**
   * Retrieves internal links of pages.
   * (<code>action=query</code>, <code>prop=links</code>).
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#links_.2F_pl">API:Properties#links</a>
   */
  public void retrieveLinks(
      EnumWikipedia wikipedia, Collection<Page> pages) throws APIException;

  /**
   * Retrieve a specific language link in a page.
   * (<code>action=query</code>, <code>prop=langlinks</code>).
   * 
   * @param from Wiki in which the article is.
   * @param to Wiki to which the link is searched.
   * @param title Page title.
   * @return Page title in the destination wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#langlinks_.2F_ll">API:Properties#langlinks</a>
   */
  public String getLanguageLink(
      EnumWikipedia from, EnumWikipedia to, String title) throws APIException;

  // ==========================================================================
  // API : Queries / Lists
  // ==========================================================================

  /**
   * Retrieves the back links of <code>page</code> and initialize redirect status.
   * (<code>action=query</code>, <code>list=backlinks</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @param redirects True if it should also retrieve links through redirects.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Backlinks">API:Backlinks</a>
   */
  public void retrieveBackLinks(
      EnumWikipedia wikipedia, Page page,
      boolean redirects) throws APIException;

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * (<code>action=query</code>, <code>list=categorymembers</code>).
   * 
   * @param wiki Wiki.
   * @param category Category.
   * @param depth Depth of lookup for sub-categories.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Categorymembers">API:Categorymembers</a>
   */
  public List<Page> retrieveCategoryMembers(
      EnumWikipedia wikipedia, String category,
      int depth, boolean limit) throws APIException;

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * (<code>action=query</code>, <code>list=embeddedin</code>).
   * 
   * @param wiki Wiki.
   * @param page Page.
   * @param namespaces Limit to some name spaces.
   * @param limit Flag indicating if the number of results should be limited.
   * @return List of pages where <code>page</code> is embedded.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Embeddedin">API:Embeddedin</a>
   */
  public List<Page> retrieveEmbeddedIn(
      EnumWikipedia wikipedia, Page page,
      List<Integer> namespaces, boolean limit) throws APIException;

  /**
   * Retrieves random pages.
   * (<code>action=query</code>, <code>list=random</code>).
   * 
   * @param wiki Wiki.
   * @param count Number of random pages.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Random">API:Random</a>
   */
  public List<Page> getRandomPages(
      EnumWikipedia wikipedia, int count) throws APIException;

  /**
   * Retrieves recent changes.
   * (<code>action=query</code>, <code>list=recentchanges</code>).
   * 
   * @param wiki Wiki.
   * @param start The timestamp to start listing from.
   * @param recentChanges The list of recent changes to be filled.
   * @return The timestamp to use as a starting point for the next call.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Recentchanges">API:Recentchanges</a>
   */
  public String getRecentChanges(
      EnumWikipedia wiki,
      String start, List<RecentChange> recentChanges) throws APIException;

  /**
   * Retrieves similar pages.
   * (<code>action=query</code>, <code>list=search</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Search">API:Search</a>
   */
  public void retrieveSimilarPages(
      EnumWikipedia wikipedia, Page page,
      boolean limit) throws APIException;

  /**
   * Retrieve raw watch list.
   * (<code>action=query</code>, <code>list=watchlistraw</code>).
   * 
   * @param wiki Wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Watchlistraw">API:Watchlistraw</a>
   */
  public List<Page> retrieveRawWatchlist(
      EnumWikipedia wiki) throws APIException;

  // ==========================================================================
  // API : Expanding templates and rendering.
  // ==========================================================================

  /**
   * Expand templates in a text.
   * (<code>action=expandtemplates</code>).
   * 
   * @param wiki Wiki.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Text with templates expanded.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Parsing_wikitext#expandtemplates">API:Parsing wikitext</a>
   */
  public String expandTemplates(EnumWikipedia wikipedia, String title, String text) throws APIException;

  /**
   * Parse text.
   * 
   * @param wikipedia Wikipedia.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Parsed text.
   * @throws APIException
   */
  public String parseText(EnumWikipedia wikipedia, String title, String text) throws APIException;

  // ==========================================================================
  // API : Purging pages' caches.
  // ==========================================================================

  /**
   * Purge the cache of <code>page</code>.
   * (<code>action=purge</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Purge">API:Purge</a>
   */
  public void purgePageCache(EnumWikipedia wikipedia, Page page) throws APIException;

  // ==========================================================================
  // API : Changing wiki content / Create and edit pages.
  // ==========================================================================

  // ==========================================================================
  // Recent changes management.
  // ==========================================================================

  /**
   * Adds a <code>RecentChangesListener</code> to the API.
   * 
   * @param wiki Wiki.
   * @param listener Recent changes listener.
   */
  public void addRecentChangesListener(
      EnumWikipedia wiki,
      RecentChangesListener listener);

  /**
   * Removes a <code>RecentChangesListener</code> from the API.
   * 
   * @param wiki Wiki.
   * @param listener Recent changes listener.
   */
  public void removeRecentChangesListener(
      EnumWikipedia wiki,
      RecentChangesListener listener);
}
