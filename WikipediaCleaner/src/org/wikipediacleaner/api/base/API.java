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

package org.wikipediacleaner.api.base;

import java.io.InputStream;
import java.util.List;

import org.apache.commons.httpclient.NameValuePair;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.QueryResult;


/**
 * MediaWiki API.
 */
/**
 * 
 */
public interface API {

  /**
   * @return Maximum number of pages per query.
   */
  public int getMaxPagesPerQuery();

  /**
   * Login into Wikipedia.
   * 
   * @param wikipedia Wikipedia URL.
   * @param username User name.
   * @param password Password.
   * @param login Flag indicating if login should be done.
   * @return Login status.
   * @throws APIException
   */
  public LoginResult login(
      EnumWikipedia wikipedia,
      String username,
      String password,
      boolean login) throws APIException;

  /**
   * Logout.
   */
  public void logout();

  /**
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param parameters Request parameters.
   * @param stream Flag indicating if the stream is needed.
   * @return Answer.
   * @throws APIException
   */
  public InputStream askToolServerPost(
      String          path,
      NameValuePair[] parameters,
      boolean         stream) throws APIException;

  /**
   * Send a GET request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param stream Flag indicating if the stream is needed.
   * @return Answer.
   * @throws APIException
   */
  public InputStream askToolServerGet(
      String          path,
      boolean         stream) throws APIException;

  /**
   * @param wikipedia Wikipedia.
   * @return The title of a random page.
   * @throws APIException
   */
  public String getRandomPage(EnumWikipedia wikipedia) throws APIException;

  /**
   * Retrieves the contents of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   */
  public void retrieveContents(EnumWikipedia wikipedia, Page page, boolean withRedirects) throws APIException;

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
   * Expand templates in a text.
   * 
   * @param wikipedia Wikipedia.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Text with templates expanded.
   * @throws APIException
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
   * Purge page cache on Wikipedia.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @throws APIException
   */
  public void purgePageCache(EnumWikipedia wikipedia, Page page) throws APIException;
  
  /**
   * Retrieves the links of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @throws APIException
   */
  public void retrieveLinks(EnumWikipedia wikipedia, Page page) throws APIException;

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
   * Retrieves the back links of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @throws APIException
   */
  public void retrieveBackLinks(EnumWikipedia wikipedia, Page page) throws APIException;

  /**
   * Retrieves the back links of <code>page</code> and initialize redirect status.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @throws APIException
   */
  public void retrieveBackLinksWithRedirects(EnumWikipedia wikipedia, Page page) throws APIException;

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @throws APIException
   */
  public void retrieveEmbeddedIn(EnumWikipedia wikipedia, Page page) throws APIException;

  /**
   * Retrieves the templates of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @throws APIException
   */
  public void retrieveTemplates(EnumWikipedia wikipedia, Page page) throws APIException;

  /**
   * Initialize the informations concerning redirects.
   * 
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(EnumWikipedia wikipedia, List<Page> pages) throws APIException;

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeDisambiguationStatus(EnumWikipedia wikipedia, List<Page> pages) throws APIException;

  /**
   * @param from Wikipedia in which the article is.
   * @param to Wikipedia to which the link is searched.
   * @param title Page title.
   * @return Page title in the destination Wikipedia.
   * @throws APIException
   */
  public String getLanguageLink(EnumWikipedia from, EnumWikipedia to, String title) throws APIException;
}
