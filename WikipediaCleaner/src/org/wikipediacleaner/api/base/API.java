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
   * @return The title of a random page.
   * @throws APIException
   */
  public String getRandomPage() throws APIException;

  /**
   * Retrieves the contents of <code>page</code>.
   * 
   * @param page Page.
   * @throws APIException
   */
  public void retrieveContents(Page page) throws APIException;

  /**
   * Expand templates in a text.
   * 
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Text with templates expanded.
   * @throws APIException
   */
  public String expandTemplates(String title, String text) throws APIException;

  /**
   * Parse text.
   * 
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Parsed text.
   * @throws APIException
   */
  public String parseText(String title, String text) throws APIException;

  /**
   * Update a page on Wikipedia.
   * 
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updatePage(Page page, String newContents, String comment) throws APIException;

  /**
   * Add a new section in a page.
   * 
   * @param page Page.
   * @param title Title of the new section.
   * @param contents Contents.
   * @param editToken Edit token.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult addNewSection(String page, String title, String contents, String editToken, boolean forceWatch) throws APIException;

  /**
   * Purge page cache on Wikipedia.
   * 
   * @param page Page.
   * @throws APIException
   */
  public void purgePageCache(Page page) throws APIException;
  
  /**
   * Retrieves the links of <code>page</code>.
   * 
   * @param page The page.
   * @throws APIException
   */
  public void retrieveLinks(Page page) throws APIException;

  /**
   * Retrieves the links of <code>page</code> and initialize redirect status.
   * 
   * @param page Page.
   * @throws APIException
   */
  public void retrieveLinksWithRedirects(Page page) throws APIException;

  /**
   * Retrieves the back links of <code>page</code>.
   * 
   * @param page The page.
   * @throws APIException
   */
  public void retrieveBackLinks(Page page) throws APIException;

  /**
   * Retrieves the back links of <code>page</code> and initialize redirect status.
   * 
   * @param page The page.
   * @throws APIException
   */
  public void retrieveBackLinksWithRedirects(Page page) throws APIException;

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * 
   * @param page Page.
   * @throws APIException
   */
  public void retrieveEmbeddedIn(Page page) throws APIException;

  /**
   * Retrieves the templates of <code>page</code>.
   * 
   * @param page The page.
   * @throws APIException
   */
  public void retrieveTemplates(Page page) throws APIException;

  /**
   * Initialize the informations concerning redirects.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(ArrayList<Page> pages) throws APIException;

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeDisambiguationStatus(ArrayList<Page> pages) throws APIException;
}
