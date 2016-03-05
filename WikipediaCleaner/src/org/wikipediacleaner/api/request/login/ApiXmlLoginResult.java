/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.login;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.JDOMParseException;
import org.jdom.xpath.XPath;
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
   * @throws APIException
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
   * @throws APIException
   */
  private LoginResult constructLogin(Element root)
      throws APIException {
    try {
      XPath xpa = XPath.newInstance("/api/login");
      Element node = (Element) xpa.selectSingleNode(root);
      if (node != null) {
        XPath xpaResult = XPath.newInstance("./@result");
        String result = xpaResult.valueOf(node);
        if ("Success".equalsIgnoreCase(result)) {
          XPath xpaUserid = XPath.newInstance("./@lguserid");
          XPath xpaUsername = XPath.newInstance("./@lgusername");
          XPath xpaToken = XPath.newInstance("./@lgtoken");
          getWiki().getConnection().setLgInformation(
              xpaToken.valueOf(node),
              xpaUsername.valueOf(node),
              xpaUserid.valueOf(node));
          return LoginResult.createCorrectLogin();
        } else if (EnumLoginResult.NEED_TOKEN.getCode().equalsIgnoreCase(result)) {
          XPath xpaToken = XPath.newInstance("./@token");
          return LoginResult.createNeedTokenLogin(xpaToken.valueOf(node));
        }
        XPath xpaWait = XPath.newInstance("./@wait");
        XPath xpaDetails = XPath.newInstance("./@details");
        return LoginResult.createErrorLogin(result, xpaDetails.valueOf(node), xpaWait.valueOf(node));
      }
    } catch (JDOMException e) {
      log.error("Error login", e);
      throw new APIException("Error parsing XML result", e);
    }
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
