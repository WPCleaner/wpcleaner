/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.request.xml;

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
import org.wikipediacleaner.api.request.ApiLoginRequest;
import org.wikipediacleaner.api.request.ApiLoginResult;
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * MediaWiki API XML login results.
 */
public class ApiXmlLoginResult extends ApiXmlResult implements ApiLoginResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlLoginResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
  }

  /**
   * Execute login request.
   * 
   * @param properties Properties defining request.
   * @return Login result.
   * @throws APIException
   */
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
          getConnectionInformation().setLgUserId(xpaUserid.valueOf(node));
          getConnectionInformation().setLgUserName(xpaUsername.valueOf(node));
          getConnectionInformation().setLgToken(xpaToken.valueOf(node));
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
}
