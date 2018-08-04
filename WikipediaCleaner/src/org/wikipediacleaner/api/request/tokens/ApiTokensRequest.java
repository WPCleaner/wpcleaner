/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.tokens;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API tokens requests.
 */
public class ApiTokensRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Type.
   */
  public final static String PROPERTY_TYPE = "type";

  /**
   * Property value for type / Block.
   */
  public final static String PROPERTY_TYPE_BLOCK = "block";

  /**
   * Property value for type / Delete.
   */
  public final static String PROPERTY_TYPE_DELETE = "delete";

  /**
   * Property value for type / Edit.
   */
  public final static String PROPERTY_TYPE_EDIT = "edit";

  /**
   * Property value for type / Email.
   */
  public final static String PROPERTY_TYPE_EMAIL = "email";

  /**
   * Property value for type / Import.
   */
  public final static String PROPERTY_TYPE_IMPORT = "import";

  /**
   * Property value for type / Move.
   */
  public final static String PROPERTY_TYPE_MOVE = "move";

  /**
   * Property value for type / Options.
   */
  public final static String PROPERTY_TYPE_OPTIONS = "options";

  /**
   * Property value for type / Patrol.
   */
  public final static String PROPERTY_TYPE_PATROL = "patrol";

  /**
   * Property value for type / Protect.
   */
  public final static String PROPERTY_TYPE_PROTECT = "protect";

  /**
   * Property value for type / Unblock.
   */
  public final static String PROPERTY_TYPE_UNBLOCK = "unblock";

  /**
   * Property value for type / Watch.
   */
  public final static String PROPERTY_TYPE_WATCH = "watch";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiTokensResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiTokensRequest(EnumWikipedia wiki, ApiTokensResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Retrieve tokens.
   * 
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveTokens() throws APIException {
    Map<String, String> properties = getProperties(ACTION_TOKENS, result.getFormat());
    User user = (getWiki().getConnection() != null) ? getWiki().getConnection().getUser() : null;
    properties.put(PROPERTY_TYPE,
        PROPERTY_TYPE_EDIT +
        ((user != null) && (user.hasRight(User.RIGHT_DELETE)) ? "|" + PROPERTY_TYPE_DELETE : ""));
    result.executeTokens(properties);
  }
}
