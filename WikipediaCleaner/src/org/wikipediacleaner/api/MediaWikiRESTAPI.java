/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api;

import java.util.List;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.api.rest.transform.RestApiTransformWikitextToLintRequest;
import org.wikipediacleaner.api.rest.transform.RestApiTransformWikitextToLintResult;


/**
 * MediaWiki REST API implementation.
 */
public class MediaWikiRESTAPI {

  /** HTTP client */
  private HttpClient httpClient;

  /**
   * Constructor.
   * 
   * @param httpClient HTTP client.
   */
  MediaWikiRESTAPI(HttpClient httpClient) {
    this.httpClient = httpClient;
  }

  /**
   * @param wiki Wiki.
   * @param title Page title.
   * @param text Wiki text to analyze.
   * @return List of errors found in the wiki text.
   * @throws APIException Exception thrown by the API.
   * @see <a href="https://fr.wikipedia.org/api/rest_v1/#!/Transforms/post_transform_wikitext_to_lint_title_revision">REST API</a>
   */
  public List<LinterError> transformWikitextToLint(EnumWikipedia wiki, String title, String text) throws APIException {
    RestApiTransformWikitextToLintResult result = new RestApiTransformWikitextToLintResult(wiki, httpClient);
    RestApiTransformWikitextToLintRequest request = new RestApiTransformWikitextToLintRequest(wiki, result);
    return request.transform(title, text);
  }
}
