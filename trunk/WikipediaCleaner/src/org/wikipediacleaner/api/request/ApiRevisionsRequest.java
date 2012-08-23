/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.api.request;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki revisions requests.
 */
public class ApiRevisionsRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Differences to.
   */
  public final static String PROPERTY_DIFFTO = "rvdiffto";

  /**
   * Property for Differences to text.
   */
  public final static String PROPERTY_DIFFTOTEXT = "rvdifftotext";

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "rvdir";

  /**
   * Property value for Direction / Newer.
   */
  public final static String PROPERTY_DIR_NEWER = "older";

  /**
   * Property value for Direction / Older.
   */
  public final static String PROPERTY_DIR_OLDER = "older";

  /**
   * Property for End Time stamp.
   */
  public final static String PROPERTY_END = "rvend";

  /**
   * Property for End Id.
   */
  public final static String PROPERTY_ENDID = "rvendid";

  /**
   * Property for Exclude user.
   */
  public final static String PROPERTY_EXCLUDEUSER = "rvexcludeuser";

  /**
   * Property for Expand templates.
   */
  public final static String PROPERTY_EXPANDTEMPLATES = "rvexpandtemplates";

  /**
   * Property for Generate XML.
   */
  public final static String PROPERTY_GENERATEXML = "rvgeneratexml";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "rvlimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "rvprop";

  /**
   * Property value for Properties / Comment.
   */
  public final static String PROPERTY_PROPERTIES_COMMENT = "comment";

  /**
   * Property value for Properties / Content.
   */
  public final static String PROPERTY_PROPERTIES_CONTENT = "content";

  /**
   * Property value for Properties / Flags.
   */
  public final static String PROPERTY_PROPERTIES_FLAGS = "flags";

  /**
   * Property value for Properties / Revision Id.
   */
  public final static String PROPERTY_PROPERTIES_IDS = "ids";

  /**
   * Property value for Properties / Parsed comment.
   */
  public final static String PROPERTY_PROPERTIES_PARSEDCOMMENT = "parsedcomment";

  /**
   * Property value for Properties / Size.
   */
  public final static String PROPERTY_PROPERTIES_SIZE = "size";

  /**
   * Property value for Properties / Tags.
   */
  public final static String PROPERTY_PROPERTIES_TAGS = "tags";

  /**
   * Property value for Properties / Time stamp.
   */
  public final static String PROPERTY_PROPERTIES_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / User.
   */
  public final static String PROPERTY_PROPERTIES_USER = "user";

  /**
   * Property for Section.
   */
  public final static String PROPERTY_SECTION = "rvsection";

  /**
   * Property for Start Time stamp.
   */
  public final static String PROPERTY_START = "rvstart";

  /**
   * Property for Start Id.
   */
  public final static String PROPERTY_STARTID = "rvstartid";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "rvtoken";

  /**
   * Property value for Token / Roll back.
   */
  public final static String PROPERTY_TOKEN_ROLLBACK = "rollback";

  /**
   * Property for User.
   */
  public final static String PROPERTY_USER = "rvuser";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiRevisionsResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiRevisionsRequest(EnumWikipedia wiki, ApiRevisionsResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load content of a page.
   * 
   * @param pages Pages for which content is requested.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   */
  public void loadContent(Collection<Page> pages, boolean withRedirects) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_PROP,
        PROPERTY_PROP_REVISIONS + "|" + PROPERTY_PROP_INFO);
    properties.put(
        PROPERTY_PROPERTIES,
        PROPERTY_PROPERTIES_CONTENT + "|" + PROPERTY_PROPERTIES_IDS + "|" + PROPERTY_PROPERTIES_TIMESTAMP);
    properties.put(
        ApiInfoRequest.PROPERTY_PROPERTIES,
        ApiInfoRequest.PROPERTY_PROPERTIES_PROTECTION);
    ConnectionInformation connection = result.getConnectionInformation();
    if ((connection != null) && (connection.getLgToken() != null)) {
      properties.put(
          ApiInfoRequest.PROPERTY_TOKEN,
          ApiInfoRequest.PROPERTY_TOKEN_EDIT);
    }
    properties.put(PROPERTY_TITLES, constructListPages(pages));
    while (result.executeLastRevision(properties, pages)) {
      //
    }

    // TODO: move this to a base class ?
    if (withRedirects) {
      List<Page> redirectPages = new ArrayList<Page>();
      for (Page page : pages) {
        if (page.isRedirect()) {
          redirectPages.add(page);
        }
      }
      if (!redirectPages.isEmpty()) {
        properties = getProperties(ACTION_QUERY, result.getFormat());
        properties.put(PROPERTY_REDIRECTS, "");
        properties.put(PROPERTY_TITLES, constructListPages(redirectPages));
        result.executeRedirect(properties, redirectPages);
      }
    }
  }
}
