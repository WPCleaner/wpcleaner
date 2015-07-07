/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

import java.util.Collection;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiInfoResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML information results.
 */
public class ApiXmlInfoResult extends ApiXmlPropertiesResult implements ApiInfoResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlInfoResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute informations request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with informations.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeInformations(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Manage redirects and missing pages
      updateRedirect(root, pages);

      // Retrieve continue
      return false;
    } catch (JDOMException e) {
      log.error("Error loading revisions", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
