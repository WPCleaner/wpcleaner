/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.templatedata;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.TemplateData;
import org.wikipediacleaner.api.data.TemplateData.InterfaceText;
import org.wikipediacleaner.api.data.TemplateData.LanguageValue;
import org.wikipediacleaner.api.data.TemplateData.Parameter;
import org.wikipediacleaner.api.request.ApiJsonResult;
import org.wikipediacleaner.api.request.ApiRequest;

import com.fasterxml.jackson.databind.JsonNode;


/**
 * MediaWiki API JSON TemplateData results.
 */
public class ApiJsonTemplateDataResult extends ApiJsonResult implements ApiTemplateDataResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiJsonTemplateDataResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute TemplateData request.
   * 
   * @param properties Properties defining request.
   * @return TemplateData for the page.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public TemplateData executeTemplateData(Map<String, String> properties)
      throws APIException {
    JsonNode root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
    TemplateData result = null;
    if (root != null) {
      Iterator<JsonNode> itPages = root.path("pages").iterator();
      while (itPages.hasNext()) {
        JsonNode page = itPages.next();
        result = createTemplateData(page);
        if (result != null) {
          return result;
        }
      }
    }
    return result;
  }

  /**
   * @param page JSON node for the page.
   * @return TemplateData.
   */
  private TemplateData createTemplateData(JsonNode page) {
    if ((page == null) || page.isMissingNode()) {
      return null;
    }
    TemplateData result = new TemplateData();
    result.setTitle(page.path("title").asText());
    result.setDescription(createInterfaceText(page.path("description")));
    List<Parameter> parameters = new ArrayList<>();
    Iterator<Entry<String, JsonNode>> itParams = page.path("params").fields();
    while (itParams.hasNext()) {
      Entry<String, JsonNode> jsonParam = itParams.next();
      JsonNode paramNode = jsonParam.getValue();
      String key = jsonParam.getKey();
      if (key != null) {
        key = key.trim();
      }
      Parameter param = new Parameter(key);
      param.setLabel(createInterfaceText(paramNode.path("label")));
      param.setDescription(createInterfaceText(paramNode.path("description")));
      param.setType(paramNode.path("type").asText());
      JsonNode aliasesNode = paramNode.path("aliases");
      if (aliasesNode.isArray()) {
        List<String> aliases = new ArrayList<>();
        Iterator<JsonNode> itAliases = aliasesNode.elements();
        while (itAliases.hasNext()) {
          aliases.add(itAliases.next().asText());
        }
        param.setAliases(aliases);
      }
      param.setRequired(paramNode.path("required").asBoolean(false));
      param.setSuggested(paramNode.path("suggested").asBoolean(false));
      JsonNode deprecatedNode = paramNode.path("deprecated");
      if (deprecatedNode.isMissingNode()) {
        param.setDeprecated(true);
      } else if (deprecatedNode.isBoolean()) {
        param.setDeprecated(deprecatedNode.asBoolean(false));
      } else {
        param.setDeprecated(deprecatedNode.asText());
      }
      param.setAutoValue(paramNode.path("autovalue").asText());
      param.setDefaultValue(paramNode.path("default").asText());
      parameters.add(param);
    }
    result.setParameters(parameters);
    return result;
  }

  /**
   * Create an InterfaceText from a JSON node.
   * 
   * @param node JSON node.
   * @return InterfaceText.
   */
  private InterfaceText createInterfaceText(JsonNode node) {
    if ((node == null) || (node.isMissingNode())) {
      return null;
    }
    List<LanguageValue> languageValues = new ArrayList<>();
    Iterator<Entry<String, JsonNode>> itValues = node.fields();
    while (itValues.hasNext()) {
      Entry<String, JsonNode> value = itValues.next();
      LanguageValue language = new LanguageValue(value.getKey(), value.getValue().asText());
      languageValues.add(language);
    }
    return new InterfaceText(languageValues);
  }
}
