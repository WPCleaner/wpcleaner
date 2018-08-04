/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.api.request.ApiJsonResult;
import org.wikipediacleaner.api.request.ApiRequest;

import com.fasterxml.jackson.databind.JsonNode;


/**
 * MediaWiki API JSON Tokens results.
 */
public class ApiJsonTokensResult extends ApiJsonResult implements ApiTokensResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiJsonTokensResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute token request.
   * 
   * @param properties Properties defining request.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void executeTokens(Map<String, String> properties)
      throws APIException {
    JsonNode root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
    if (root != null) {
      JsonNode tokens = root.path("query").path("tokens");
      JsonNode csrfNode = tokens.get(ApiTokensRequest.TOKEN_CSRF);
      if (csrfNode != null) {
        String csrfToken = csrfNode.textValue();
        getWiki().getConnection().setEditToken(csrfToken);
        User user = getWiki().getConnection().getUser();
        if ((user != null) && user.hasRight(User.RIGHT_DELETE)) {
          getWiki().getConnection().setDeleteToken(csrfToken);
        }
      }
      JsonNode loginNode = tokens.get(ApiTokensRequest.TOKEN_LOGIN);
      if (loginNode != null) {
        String loginToken = loginNode.textValue();
        getWiki().getConnection().setLoginToken(loginToken);
      }
    }
  }
}
