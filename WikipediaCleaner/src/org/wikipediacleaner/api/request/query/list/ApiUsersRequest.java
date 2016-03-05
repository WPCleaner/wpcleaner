/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.User;


/**
 * MediaWiki API users requests.
 */
public class ApiUsersRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "usprop";

  /**
   * Property value for properties / Block info.
   */
  public final static String PROPERTY_PROP_BLOCKINFO = "blockinfo";

  /**
   * Property value for properties / Edit count.
   */
  public final static String PROPERTY_PROP_EDITCOUNT = "editcount";

  /**
   * Property value for properties / Emailable.
   */
  public final static String PROPERTY_PROP_EMAILABLE = "emailable";

  /**
   * Property value for properties / Gender.
   */
  public final static String PROPERTY_PROP_GENDER = "gender";

  /**
   * Property value for properties / Groups.
   */
  public final static String PROPERTY_PROP_GROUPS = "groups";

  /**
   * Property value for properties / Implicit groups.
   */
  public final static String PROPERTY_PROP_IMPLICITGROUPS = "implicitgroups";

  /**
   * Property value for properties / Registration.
   */
  public final static String PROPERTY_PROP_REGISTRATION = "registration";

  /**
   * Property value for properties / Rights.
   */
  public final static String PROPERTY_PROP_RIGHTS = "rights";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "ustoken";

  /**
   * Property value for token / User rights.
   */
  public final static String PROPERTY_TOKEN_USERRIGHTS = "userrights";

  /**
   * Property for Users.
   */
  public final static String PROPERTY_USERS = "ususers";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiUsersResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiUsersRequest(EnumWikipedia wiki, ApiUsersResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Retrieve User information.
   */
  public User retrieveUser(String username) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_LIST, PROPERTY_LIST_USERS);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_USERS, username);
    properties.put(PROPERTY_PROP, PROPERTY_PROP_GROUPS + "|" + PROPERTY_PROP_RIGHTS);
    return result.executeUser(properties);
  }
}
