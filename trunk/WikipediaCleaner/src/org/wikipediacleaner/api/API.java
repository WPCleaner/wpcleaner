/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumQueryPage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AbuseFilter;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.api.data.RecentChange;
import org.wikipediacleaner.api.data.Section;
import org.wikipediacleaner.api.data.User;


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
   * @param wiki Wiki.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(EnumWikipedia wiki, List<Page> pages) throws APIException;

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
      EnumWikipedia wiki,
      String username,
      String password,
      boolean login) throws APIException;

  /**
   * Logout.
   * (<code>action=logout</code>).
   * 
   * @param wiki Wiki.
   * @see <a href="http://www.mediawiki.org/wiki/API:Logout">API:Logout</a>
   */
  public void logout(EnumWikipedia wiki);

  /**
   * Retrieve tokens.
   * (<code>action=tokens</code>).
   * 
   * @param wiki Wiki.
   * @throws APIException
   */
  public void retrieveTokens(EnumWikipedia wiki) throws APIException;

  // ==========================================================================
  // API : Queries / Meta information
  // ==========================================================================

  // ==========================================================================
  // API : Queries / All messages
  // ==========================================================================

  /**
   * Load messages.
   * (<code>action=query</code>, <code>meta=allmessages</code>).
   * 
   * @param wiki Wiki.
   * @param messageName Message name.
   * @throws APIException
   * @see <a href="https://www.mediawiki.org/wiki/API:Allmessages">API:Allmessages</a>
   */
  public String loadMessage(EnumWikipedia wiki, String messageName) throws APIException;

  // ==========================================================================
  // API : Queries / Properties
  // ==========================================================================

  /**
   * Retrieves the informations of a list of pages.
   * (<code>action=query</code>, <code>prop=info</code>).
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#info_.2F_in">API:Properties#info</a>
   */
  public void retrieveInfo(
      EnumWikipedia wiki,
      Collection<Page> pages) throws APIException;

  /**
   * Retrieves the contents of a list of pages.
   * (<code>action=query</code>, <code>prop=revisions</code>).
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @param usePageId True if page identifiers should be used.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#revisions_.2F_rv">API:Properties#revisions</a>
   */
  public void retrieveContents(
      EnumWikipedia wiki,
      Collection<Page> pages, boolean usePageId,
      boolean withRedirects) throws APIException;

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * (<code>action=query</code>, <code>prop=categories</code>) or
   * (<code>action=query</code>, <code>prop=templates</code>).
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @param forceApiCall True if API call should be forced even if the list of disambiguation pages is loaded.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#categories_.2F_cl">API:Properties#categories</a>
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#templates_.2F_tl">API:Properties#templates</a>
   */
  public void initializeDisambiguationStatus(
      EnumWikipedia wiki, List<Page> pages, boolean forceApiCall) throws APIException;

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
      EnumWikipedia wiki, Collection<Page> pages) throws APIException;

  /**
   * Retrieves internal links of one page.
   * (<code>action=query</code>, <code>prop=links</code>).
   * 
   * @param wiki Wiki.
   * @param page Page.
   * @param namespace Restrict the list to a given namespace.
   * @param knownPages Already known pages.
   * @param redirects True if redirects are requested.
   * @param disambigNeeded True if disambiguation information is needed.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#links_.2F_pl">API:Properties#links</a>
   */
  public void retrieveLinks(
      EnumWikipedia wiki, Page page, Integer namespace,
      List<Page> knownPages,
      boolean redirects, boolean disambigNeeded) throws APIException;

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
   * Retrieves the list of abuse filters.
   * (<code>action=query</code>, <code>list=abusefilters</code>).
   * 
   * @param wiki Wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Backlinks">API:Backlinks</a>
   */
  public List<AbuseFilter> retrieveAbuseFilters(
      EnumWikipedia wiki) throws APIException;

  /**
   * Retrieves the abuse log for a filter.
   * (<code>action=query</code>, <code>list=abuselog</code>).
   * 
   * @param wiki Wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Abuselog">API:Abuselog</a>
   */
  public List<Page> retrieveAbuseLog(
      EnumWikipedia wiki, AbuseFilter filter) throws APIException;

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
      EnumWikipedia wiki, Page page,
      boolean redirects) throws APIException;

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * (<code>action=query</code>, <code>list=categorymembers</code>).
   * 
   * @param wiki Wiki.
   * @param category Category.
   * @param depth Depth of lookup for sub-categories.
   * @param limit Flag indicating if the number of results should be limited.
   * @param max Absolute maximum number of results
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Categorymembers">API:Categorymembers</a>
   */
  public void retrieveCategoryMembers(
      EnumWikipedia wiki, Page category,
      int depth, boolean limit, int max) throws APIException;

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * (<code>action=query</code>, <code>list=embeddedin</code>).
   * 
   * @param wiki Wiki.
   * @param page Page.
   * @param namespaces Limit to some namespaces.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Embeddedin">API:Embeddedin</a>
   */
  public void retrieveEmbeddedIn(
      EnumWikipedia wiki, Page page,
      List<Integer> namespaces, boolean limit) throws APIException;

  /**
   * Retrieves the pages which have a given property.
   * (<code>action=query</code>, <code>list=pageswithprop</code>).
   * 
   * @param wiki Wiki.
   * @param property Property name.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Pageswithprop">API:Pageswithprop</a>
   */
  public List<Page> retrievePagesWithProp(
      EnumWikipedia wiki,
      String property, boolean limit) throws APIException;

  /**
   * Retrieves the pages which are protected in creation indefinitely.
   * (<code>action=query</code>, <code>list=protectedtitles</code>).
   * 
   * @param wiki Wiki.
   * @param namespaces Limit to some namespaces.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Protectedtitles">API:Protectedtitles</a>
   */
  public List<Page> getProtectedTitles(
      EnumWikipedia wiki,
      List<Integer> namespaces, boolean limit) throws APIException;

  /**
   * Retrieves a special list of pages.
   * (<code>action=query</code>, <code>list=querypage</code>).
   * 
   * @param wiki Wiki.
   * @param query Type of list.
   * @return List of pages depending on the query.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Querypage">API:Querypage</a>
   */
  public List<Page> getQueryPages(
      EnumWikipedia wiki, EnumQueryPage query) throws APIException;

  /**
   * Retrieves random pages.
   * (<code>action=query</code>, <code>list=random</code>).
   * 
   * @param wiki Wiki.
   * @param count Number of random pages.
   * @param redirects True if redirect pages are requested.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Random">API:Random</a>
   */
  public List<Page> getRandomPages(
      EnumWikipedia wiki, int count,
      boolean redirects) throws APIException;

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
      EnumWikipedia wiki, Page page,
      boolean limit) throws APIException;

  /**
   * Retrieve user information.
   * (<code>action=query</code>, <code>list=users</code>).
   * 
   * @param wiki Wiki.
   * @param name User name.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Users">API:Users</a>
   */
  public User retrieveUser(
      EnumWikipedia wiki, String name) throws APIException;

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
  public String expandTemplates(
      EnumWikipedia wiki, String title, String text) throws APIException;

  /**
   * Parse text.
   * (<code>action=parse</code>).
   * 
   * @param wiki Wiki.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Parsed text.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Parsing_wikitext#parse">API:Parsing wikitext</a>
   */
  public String parseText(
      EnumWikipedia wiki, String title, String text) throws APIException;

  /**
   * Retrieve list of sections.
   * (<code>action=parse</code>).
   * 
   * @param wiki Wiki.
   * @param page Page.
   * @return List of sections.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Parsing_wikitext#parse">API:Parsing wikitext</a>
   */
  public List<Section> retrieveSections(
      EnumWikipedia wiki, Page page) throws APIException;

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
  public void purgePageCache(
      EnumWikipedia wiki, Page page) throws APIException;

  // ==========================================================================
  // API : Changing wiki content / Create and edit pages.
  // ==========================================================================

  // ==========================================================================
  // API : Changing wiki content / Create and edit pages.
  // ==========================================================================

  /**
   * Delete the <code>page</code>.
   * (<code>action=delete</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @param reason Reason for deleting the page.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Delete">API:Delete</a>
   */
  public void deletePage(
      EnumWikipedia wiki, Page page, String reason) throws APIException;

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
