/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki abuse log requests.
 */
public class ApiAbuseLogRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "afldir";

  /**
   * Property value for Direction / Newer.
   */
  public final static String PROPERTY_DIR_NEWER = "newer";

  /**
   * Property value for Direction / Older.
   */
  public final static String PROPERTY_DIR_OLDER = "older";

  /**
   * Property for End.
   */
  public final static String PROPERTY_END = "aflend";

  /**
   * Property for Filter.
   */
  public final static String PROPERTY_FILTER = "aflfilter";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "afllimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "aflprop";

  /**
   * Property value for Properties / Action.
   */
  public final static String PROPERTY_PROP_ACTION = "action";

  /**
   * Property value for Properties / Details.
   */
  public final static String PROPERTY_PROP_DETAILS = "details";

  /**
   * Property value for Properties / Filter.
   */
  public final static String PROPERTY_PROP_FILTER = "filter";

  /**
   * Property value for Properties / Hidden.
   */
  public final static String PROPERTY_PROP_HIDDEN = "hidden";

  /**
   * Property value for Properties / Identifiers.
   */
  public final static String PROPERTY_PROP_IDS = "ids";

  /**
   * Property value for Properties / IP.
   */
  public final static String PROPERTY_PROP_IP = "ip";

  /**
   * Property value for Properties / Result.
   */
  public final static String PROPERTY_PROP_RESULT = "result";

  /**
   * Property value for Properties / Timestamp.
   */
  public final static String PROPERTY_PROP_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROP_TITLE = "title";

  /**
   * Property value for Properties / User.
   */
  public final static String PROPERTY_PROP_USER = "user";

  /**
   * Property for Start.
   */
  public final static String PROPERTY_START = "aflstart";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "afltitle";

  /**
   * Property for User.
   */
  public final static String PROPERTY_USER = "afluser";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiAbuseLogResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiAbuseLogRequest(EnumWikipedia wiki, ApiAbuseLogResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of pages that triggered an abuse filters.
   * 
   * @param filterId Filter identifier.
   * @param maxDuration Maximum number of days.
   * @return List of pages that triggered that filter.
   */
  public List<Page> loadAbuseLog(
      Integer filterId, Integer maxDuration) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_ABUSELOG);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    if (filterId != null) {
      properties.put(PROPERTY_FILTER, Integer.toString(filterId));
    }
    if (maxDuration != null) {
      Calendar calendar = new GregorianCalendar();
      calendar.add(Calendar.DAY_OF_MONTH, -maxDuration.intValue());
      SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
      String formattedDate = df.format(new Date(calendar.getTimeInMillis()));
      properties.put(PROPERTY_END, formattedDate);
    }
    List<Page> list = new ArrayList<Page>();
    while (result.executeAbuseLog(properties, list)) {
      //
    }
    return list;
  }
}
