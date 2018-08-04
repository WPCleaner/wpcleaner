/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * MediaWiki API tokens requests.
 */
public class ApiTokensRequest extends ApiMetaRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /** Property for Type */
  public final static String PROPERTY_TYPE = "type";

  /** Property for Type / Create account */
  public final static String PROPERTY_TYPE_CREATE_ACCOUNT = "createaccount";

  /** Property for Type / CSRF */
  public final static String PROPERTY_TYPE_CSRF = "csrf";

  /** Property for Type / Login */
  public final static String PROPERTY_TYPE_LOGIN = "login";

  /** Property for Type / Patrol */
  public final static String PROPERTY_TYPE_PATROL = "patrol";

  /** Property for Type / Roll back */
  public final static String PROPERTY_TYPE_ROLLBACK = "rollback";

  /** Property for Type / User rights */
  public final static String PROPERTY_TYPE_USER_RIGHTS = "userrights";

  /** Property for Type / Watch */
  public final static String PROPERTY_TYPE_WATCH = "watch";

  // ==========================================================================
  // Token names
  // ==========================================================================

  /** Create account token */
  public final static String TOKEN_CREATE_ACCOUNT = "createaccounttoken";

  /** CSRF token */
  public final static String TOKEN_CSRF = "csrftoken";

  /** Login token */
  public final static String TOKEN_LOGIN = "logintoken";

  /** Patrol token */
  public final static String TOKEN_PATROL = "patroltoken";

  /** Roll back token */
  public final static String TOKEN_ROLLBACK = "rollbacktoken";

  /** User rights token */
  public final static String TOKEN_USER_RIGHTS = "userrightstoken";

  /** Watch token */
  public final static String TOKEN_WATCH = "watchtoken";

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
   * Load one token.
   * 
   * @param tokenName Name of the token to retrieve.
   * @throws APIException Exception thrown by the API.
   */
  public void retrieveToken(
      String tokenName) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_META,
        PROPERTY_META_TOKENS);
    properties.put(PROPERTY_TYPE, tokenName);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    result.executeTokens(properties);
  }
}
