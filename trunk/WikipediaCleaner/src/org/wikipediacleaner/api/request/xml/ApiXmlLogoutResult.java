/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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
import org.jdom.input.JDOMParseException;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiLogoutResult;

/**
 * MediaWiki API XML logout results.
 */
public class ApiXmlLogoutResult extends ApiXmlResult implements ApiLogoutResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLogoutResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute logout request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public void executeLogout(
      Map<String, String> properties)
          throws APIException {
    try {
      getRoot(properties, 1);
    } catch (JDOMParseException e) {
      log.error("Exception in MediaWikiAPI.logout()", e);
      throw new APIException("Couldn't logout");
    }
  }

  /**
   * @return True if identification parameters should be sent.
   */
  @Override
  protected boolean shouldSendIdentification() {
    return true;
  }
}
