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
import org.jdom.input.JDOMParseException;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiPurgeResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML purge results.
 */
public class ApiXmlPurgeResult extends ApiXmlResult implements ApiPurgeResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlPurgeResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute purge request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public void executePurge(
      Map<String, String> properties)
          throws APIException {
    try {
      checkForError(getRoot(properties, ApiRequest.MAX_ATTEMPTS));
    } catch (JDOMParseException e) {
      log.error("Error purging page cache", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
