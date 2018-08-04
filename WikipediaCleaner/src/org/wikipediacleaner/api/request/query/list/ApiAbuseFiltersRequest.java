/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AbuseFilter;


/**
 * MediaWiki abuse filters requests.
 */
public class ApiAbuseFiltersRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "abfdir";

  /**
   * Property value for Direction / Newer.
   */
  public final static String PROPERTY_DIR_NEWER = "newer";

  /**
   * Property value for Direction / Older.
   */
  public final static String PROPERTY_DIR_OLDER = "older";

  /**
   * Property for End Identifier.
   */
  public final static String PROPERTY_ENDID = "abfendid";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "abflimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "abfprop";

  /**
   * Property value for Properties / Actions.
   */
  public final static String PROPERTY_PROP_ACTIONS = "actions";

  /**
   * Property value for Properties / Comments.
   */
  public final static String PROPERTY_PROP_COMMENTS = "comments";

  /**
   * Property value for Properties / Description.
   */
  public final static String PROPERTY_PROP_DESC = "description";

  /**
   * Property value for Properties / Hits.
   */
  public final static String PROPERTY_PROP_HITS = "hits";

  /**
   * Property value for Properties / ID.
   */
  public final static String PROPERTY_PROP_ID = "id";

  /**
   * Property value for Properties / Last Editor.
   */
  public final static String PROPERTY_PROP_LASTEDITOR = "lasteditor";

  /**
   * Property value for Properties / Last Edit Time.
   */
  public final static String PROPERTY_PROP_LASTEDITTIME = "lastedittime";

  /**
   * Property value for Properties / Pattern.
   */
  public final static String PROPERTY_PROP_PATTERN = "pattern";

  /**
   * Property value for Properties / Private.
   */
  public final static String PROPERTY_PROP_PRIVATE = "private";

  /**
   * Property value for Properties / Status.
   */
  public final static String PROPERTY_PROP_STATUS = "status";

  /**
   * Property for Show.
   */
  public final static String PROPERTY_SHOW = "abfshow";

  /**
   * Property value for Show / Deleted.
   */
  public final static String PROPERTY_SHOW_DELETED = "deleted";

  /**
   * Property value for Show / Not Deleted.
   */
  public final static String PROPERTY_SHOW_NOTDELETED = "!deleted";

  /**
   * Property value for Show / Enabled.
   */
  public final static String PROPERTY_SHOW_ENABLED = "enabled";

  /**
   * Property value for Show / Not Enabled.
   */
  public final static String PROPERTY_SHOW_NOTENABLED = "!enabled";

  /**
   * Property value for Show / Private.
   */
  public final static String PROPERTY_SHOW_PRIVATE = "private";

  /**
   * Property value for Show / Not Private.
   */
  public final static String PROPERTY_SHOW_NOTPRIVATE = "!private";

  /**
   * Property for Start Identifier.
   */
  public final static String PROPERTY_STARTID = "abfstartid";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiAbuseFiltersResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiAbuseFiltersRequest(EnumWikipedia wiki, ApiAbuseFiltersResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of abuse filters.
   * 
   * @return List of abuse filters.
   * @throws APIException Exception thrown by the API.
   */
  public List<AbuseFilter> loadAbuseFilters() throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_ABUSEFILTERS);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_SHOW, PROPERTY_SHOW_ENABLED);
    List<AbuseFilter> list = new ArrayList<AbuseFilter>();
    while (result.executeAbuseFilters(properties, list)) {
      //
    }
    Collections.sort(list);
    return list;
  }
}
