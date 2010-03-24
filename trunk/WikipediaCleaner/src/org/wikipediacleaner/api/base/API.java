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

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Namespace;
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
   * @param id Namespace Id.
   * @return Name of the namespace.
   */
  public String getNamespace(Integer id);

  /**
   * Retrieves the list of Namespaces.
   * 
   * @param list List of Namespaces. 
   */
  public void getNamespaces(List<Namespace> list);

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
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updatePage(EnumWikipedia wikipedia, Page page, String newContents, String comment) throws APIException;

  /**
   * Add a new section in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param title Title of the new section.
   * @param contents Contents.
   * @param editToken Edit token.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult addNewSection(
      EnumWikipedia wikipedia,
      String page, String title, String contents, String editToken, boolean forceWatch) throws APIException;

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
   * @param knownPages Already known pages.
   * @throws APIException
   */
  public void retrieveLinksWithRedirects(
      EnumWikipedia wikipedia,
      Page page, ArrayList<Page> knownPages) throws APIException;

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
  public void initializeRedirect(EnumWikipedia wikipedia, ArrayList<Page> pages) throws APIException;

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeDisambiguationStatus(EnumWikipedia wikipedia, ArrayList<Page> pages) throws APIException;
}
