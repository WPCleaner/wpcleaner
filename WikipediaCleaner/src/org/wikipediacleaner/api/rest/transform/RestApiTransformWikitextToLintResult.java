/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.rest.transform;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.api.rest.RestApiJsonResult;
import org.wikipediacleaner.api.rest.RestApiRequest;

import com.fasterxml.jackson.databind.JsonNode;


/**
 * MediaWiki REST API "Transform Wikitext To Lint" results.
 */
public class RestApiTransformWikitextToLintResult extends RestApiJsonResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public RestApiTransformWikitextToLintResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * @param title Page title.
   * @param properties Properties.
   * @return List of errors detected by Linter.
   * @throws APIException Exception thrown by the API.
   */
  public List<LinterError> transform(String title, Map<String, String> properties) throws APIException {
    List<LinterError> result = new ArrayList<>();
    String url = "api/rest_v1/transform/wikitext/to/lint";
    String formattedTitle = title;
    if (formattedTitle != null) {
      formattedTitle = formattedTitle.replaceAll(" ", "_");
    }
    JsonNode root = getRoot(properties, url, formattedTitle, RestApiRequest.MAX_ATTEMPTS);
    if (root != null) {
      Iterator<JsonNode> itErrors = root.elements();
      while (itErrors.hasNext()) {
        JsonNode error = itErrors.next();
        boolean ok = true;
        int start = 0;
        int end = 0;

        // Type of error
        String type = null;
        JsonNode nodeType = error.path("type");
        if (nodeType != null) {
          type = nodeType.asText();
        } else {
          ok = false;
        }

        // Position
        JsonNode dsrNode = error.path("dsr");
        if ((dsrNode != null) && dsrNode.isArray()) {
          int count = 0;
          for (final JsonNode node : dsrNode) {
            if (count == 0) {
              start = node.asInt();
            } else if (count == 1) {
              end = node.asInt();
            }
            count++;
          }
        } else {
          ok = false;
        }

        // Parameters
        Map<String, String> params = new HashMap<String, String>();
        JsonNode paramsNode = error.path("params");
        if ((paramsNode != null) && paramsNode.isObject()) {
          Iterator<String> itNames = paramsNode.fieldNames();
          while (itNames.hasNext()) {
            String name = itNames.next();
            if (name != null) {
              JsonNode node = paramsNode.get(name);
              params.put(name, node.asText());
            }
          }
        }

        // Template information
        boolean multiPartTemplateBlock = false;
        String templateName = null;
        JsonNode templateInfoNode = error.path("templateInfo");
        if (templateInfoNode != null) {
          JsonNode multiPartNode = templateInfoNode.get("multiPartTemplateBlock");
          if (multiPartNode != null) {
            multiPartTemplateBlock = multiPartNode.asBoolean(false);
          }
          JsonNode templateNameNode = templateInfoNode.get("name");
          if (templateNameNode != null) {
            templateName = templateNameNode.asText();
          }
        }

        // Create error
        if (ok) {
          result.add(new LinterError(
              title, type, params, start, end,
              multiPartTemplateBlock, templateName));
        }
      }
    }
    return result;
  }
}
