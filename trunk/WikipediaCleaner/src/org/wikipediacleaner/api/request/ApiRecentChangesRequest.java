/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.RecentChange;


/**
 * MediaWiki recent changes requests.
 */
public class ApiRecentChangesRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "rcdir";

  /**
   * Property value for direction / Newer (oldest changes first).
   */
  public final static String PROPERTY_DIR_NEWER = "newer";

  /**
   * Property value for direction / Older (newest changes first).
   */
  public final static String PROPERTY_DIR_OLDER = "older";

  /**
   * Property for End.
   */
  public final static String PROPERTY_END = "rcend";

  /**
   * Property for Exclude User.
   */
  public final static String PROPERTY_EXCLUDEUSER = "rcexcludeuser";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "rclimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "rcnamespace";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "rcprop";

  /**
   * Property value for Properties / Comment.
   */
  public final static String PROPERTY_PROPERTIES_COMMENT = "comment";

  /**
   * Property value for Properties / Flags.
   */
  public final static String PROPERTY_PROPERTIES_FLAGS = "flags";

  /**
   * Property value for Properties / Identifiers.
   */
  public final static String PROPERTY_PROPERTIES_IDS = "ids";

  /**
   * Property value for Properties / Log info.
   */
  public final static String PROPERTY_PROPERTIES_LOGINFO = "loginfo";

  /**
   * Property value for Properties / Patrolled.
   */
  public final static String PROPERTY_PROPERTIES_PATROLLED = "patrolled";

  /**
   * Property value for Properties / Redirect.
   */
  public final static String PROPERTY_PROPERTIES_REDIRECT = "redirect";

  /**
   * Property value for Properties / Sizes.
   */
  public final static String PROPERTY_PROPERTIES_SIZES = "sizes";

  /**
   * Property value for Properties / Timestamp.
   */
  public final static String PROPERTY_PROPERTIES_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROPERTIES_TITLE = "title";

  /**
   * Property value for Properties / User.
   */
  public final static String PROPERTY_PROPERTIES_USER = "user";

  /**
   * Property for Criteria.
   */
  public final static String PROPERTY_SHOW = "rcshow";

  /**
   * Property value for criteria / Anonymous edits.
   */
  public final static String PROPERTY_SHOW_ANONYMOUS = "anon";

  /**
   * Property value for criteria / Non anonymous edits.
   */
  public final static String PROPERTY_SHOW_NOTANONYMOUS = "!anon";

  /**
   * Property value for criteria / Bot edits.
   */
  public final static String PROPERTY_SHOW_BOT = "bot";

  /**
   * Property value for criteria / Non bot edits.
   */
  public final static String PROPERTY_SHOW_NOTBOT = "!bot";

  /**
   * Property value for criteria / Minor edits.
   */
  public final static String PROPERTY_SHOW_MINOR = "minor";

  /**
   * Property value for criteria / Non minor edits.
   */
  public final static String PROPERTY_SHOW_NOTMINOR = "!minor";

  /**
   * Property value for criteria / Patrolled edits.
   */
  public final static String PROPERTY_SHOW_PATROLLED = "patrolled";

  /**
   * Property value for criteria / Non patrolled edits.
   */
  public final static String PROPERTY_SHOW_NOTPATROLLED = "!patrolled";

  /**
   * Property value for criteria / Redirects.
   */
  public final static String PROPERTY_SHOW_REDIRECT = "redirect";

  /**
   * Property value for criteria / Non redirects.
   */
  public final static String PROPERTY_SHOW_NOTREDIRECT = "!redirect";

  /**
   * Property for Start.
   */
  public final static String PROPERTY_START = "rcstart";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "rctoken";

  /**
   * Property for Top only changes.
   */
  public final static String PROPERTY_TOPONLY = "rctoponly";

  /**
   * Property for Type of change.
   */
  public final static String PROPERTY_TYPE = "rctype";

  /**
   * Property value for type of change / Regular page edits.
   */
  public final static String PROPERTY_TYPE_EDIT = "edit";

  /**
   * Property value for type of change / Log entries.
   */
  public final static String PROPERTY_TYPE_LOG = "log";

  /**
   * Property value for type of change / Page creations.
   */
  public final static String PROPERTY_TYPE_NEW = "new";

  /**
   * Property for User.
   */
  public final static String PROPERTY_USER = "rcuser";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiRecentChangesResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiRecentChangesRequest(EnumWikipedia wiki, ApiRecentChangesResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of recent changes.
   * 
   * @param start The timestamp to start listing from.
   * @param recentChanges The list of recent changes to be filled.
   * @return The timestamp to use as a starting point for the next call.
   */
  public String loadRecentChanges(String start, List<RecentChange> recentChanges) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_RECENTCHANGES);
    properties.put(
        PROPERTY_PROPERTIES,
        PROPERTY_PROPERTIES_COMMENT + "|" +
        PROPERTY_PROPERTIES_FLAGS + "|" +
        PROPERTY_PROPERTIES_IDS + "|" +
        PROPERTY_PROPERTIES_LOGINFO + "|" +
        PROPERTY_PROPERTIES_REDIRECT + "|" +
        PROPERTY_PROPERTIES_TIMESTAMP + "|" +
        PROPERTY_PROPERTIES_TITLE + "|" +
        PROPERTY_PROPERTIES_USER);
    if (start != null) {
      properties.put(PROPERTY_END, start);
    }
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    String nextStart = result.executeRecentChanges(properties, recentChanges);
    return nextStart;
  }
}
