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

package org.wikipediacleaner.api.request;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Base interface for MediaWiki API results.
 */
public interface ApiResult {

  /**
   * @return Format of the XML result.
   */
  public String getFormat();

  /**
   * @return Wiki on which requests are made.
   */
  public EnumWikipedia getWiki();

  /**
   * @return HTTP client for making requests.
   */
  public HttpClient getHttpClient();
}
