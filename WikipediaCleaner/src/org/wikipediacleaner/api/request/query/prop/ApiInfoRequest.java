/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki information requests.
 */
public class ApiInfoRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "inprop";

  /**
   * Property value for Properties / Pre load.
   */
  public final static String PROPERTY_PROPERTIES_PRELOAD = "preload";

  /**
   * Property value for Properties / Protection.
   */
  public final static String PROPERTY_PROPERTIES_PROTECTION = "protection";

  /**
   * Property value for Properties / Subject Id.
   */
  public final static String PROPERTY_PROPERTIES_SUBJECTID = "subjectid";

  /**
   * Property value for Properties / Talk id.
   */
  public final static String PROPERTY_PROPERTIES_TALKID = "talkid";

  /**
   * Property value for Properties / URL.
   */
  public final static String PROPERTY_PROPERTIES_URL = "url";

  /**
   * Property value for Properties / Watched.
   */
  public final static String PROPERTY_PROPERTIES_WATCHED = "watched";

  /**
   * Property for Testing actions.
   */
  public final static String PROPERTY_TEST_ACTIONS = "intestactions";

  /**
   * Property value for Testing actions / Edit.
   */
  public final static String PROPERTY_TEST_ACTIONS_EDIT = "edit";

  /**
   * Property for Testing actions details.
   */
  public final static String PROPERTY_TEST_ACTIONS_DETAIL = "intestactionsdetail";

  /**
   * Property value for Testing actions details / Boolean.
   */
  public final static String PROPERTY_TEST_ACTIONS_DETAIL_BOOLEAN = "boolean";

  /**
   * Property value for Testing actions details / Full.
   */
  public final static String PROPERTY_TEST_ACTIONS_DETAIL_FULL = "full";

  /**
   * Property value for Testing actions details / Quick.
   */
  public final static String PROPERTY_TEST_ACTIONS_DETAIL_QUICK = "quick";

  /**
   * Property for Token.
   */
  @Deprecated
  public final static String PROPERTY_TOKEN = "intoken";

  /**
   * Property value for Token / Delete.
   */
  @Deprecated
  public final static String PROPERTY_TOKEN_DELETE = "delete";

  /**
   * Property value for Token / Edit.
   */
  @Deprecated
  public final static String PROPERTY_TOKEN_EDIT = "edit";

  /**
   * Property value for Token / Move.
   */
  @Deprecated
  public final static String PROPERTY_TOKEN_MOVE = "move";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiInfoResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiInfoRequest(EnumWikipedia wiki, ApiInfoResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load informations of a list of pages.
   * 
   * @param pages List of pages.
   * @throws APIException Exception thrown by the API.
   */
  public void loadInformations(Collection<Page> pages) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_PROP,
        PROPERTY_PROP_REVISIONS + "|" + PROPERTY_PROP_INFO);
    if (getWiki().getWikiConfiguration().isTranslatable()) {
      properties.put(
          PROPERTY_TEST_ACTIONS,
          PROPERTY_TEST_ACTIONS_EDIT);
      properties.put(
          PROPERTY_TEST_ACTIONS_DETAIL,
          PROPERTY_TEST_ACTIONS_DETAIL_FULL);
    }
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    List<Collection<Page>> tmpPages = splitListPages(pages, MAX_PAGES_PER_QUERY);
    for (Collection<Page> tmpPages2 : tmpPages) {
      properties.put(PROPERTY_TITLES, constructListTitles(tmpPages2));
      while (result.executeInformations(properties, tmpPages2)) {
        //
      }
    }
  }
}
