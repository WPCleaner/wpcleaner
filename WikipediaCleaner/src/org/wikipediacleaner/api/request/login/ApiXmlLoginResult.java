/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.login;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
import org.jdom2.filter.Filters;
import org.jdom2.input.JDOMParseException;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumLoginResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML login results.
 */
public class ApiXmlLoginResult extends ApiXmlResult implements ApiLoginResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLoginResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute login request.
   * 
   * @param properties Properties defining request.
   * @return Login result.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public LoginResult executeLogin(
      Map<String, String> properties)
          throws APIException {
    try {
      LoginResult result = constructLogin(getRoot(properties, 1));
      if ((result != null) && (result.isTokenNeeded())) {
        properties.put(ApiLoginRequest.PROPERTY_TOKEN, result.getDetails());
        result = constructLogin(getRoot(properties, 1));
      }
      return result;
    } catch (JDOMParseException e) {
      log.error("Exception in MediaWikiAPI.login()", e);
      throw new APIException("Couldn't login");
    }
  }

  /**
   * Analyze login answer.
   * 
   * @param root Root element in MediaWiki answer.
   * @return Result of the login.
   * @throws APIException Exception thrown by the API.
   */
  private LoginResult constructLogin(Element root)
      throws APIException {
//    try {
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/login", Filters.element());
      Element node = xpa.evaluateFirst(root);
      if (node != null) {
        String result = node.getAttributeValue("result");
        if ("Success".equalsIgnoreCase(result)) {
          getWiki().getConnection().setLgInformation(
              node.getAttributeValue("lgtoken"),
              node.getAttributeValue("lgusername"),
              node.getAttributeValue("lguserid"));
          return LoginResult.createCorrectLogin();
        } else if (EnumLoginResult.NEED_TOKEN.getCode().equalsIgnoreCase(result)) {
          return LoginResult.createNeedTokenLogin(node.getAttributeValue("token"));
        }
        return LoginResult.createErrorLogin(
            result, node.getAttributeValue("details"), node.getAttributeValue("wait"));
      }
//    } catch (JDOMException e) {
//      log.error("Error login", e);
//      throw new APIException("Error parsing XML result", e);
//    }
    return LoginResult.createErrorLogin(null, null, null);
  }

  /**
   * @return True if identification parameters should be sent.
   */
  @Override
  protected boolean shouldSendIdentification() {
    return true;
  }
}
