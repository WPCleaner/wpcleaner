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

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumQueryResult;


/**
 * Class containing the information about the query result.
 */
public class QueryResult {

  private EnumQueryResult queryResult;
  private String details;
  private String wait;

  private Integer pageId;
  private String  pageTitle;
  private Integer pageOldRevId;
  private Integer pageNewRevId;

  static public QueryResult createCorrectQuery(
      Integer pageId, String pageTitle, Integer pageOldRevId, Integer pageNewRevId) {
    return new QueryResult(
        EnumQueryResult.SUCCESS, null, null,
        pageId, pageTitle, pageOldRevId, pageNewRevId);
  }

  static public QueryResult createErrorQuery(
      String errorText, String details, String wait) {
    EnumQueryResult queryResult = EnumQueryResult.getEnumByCode(errorText);
    return new QueryResult(queryResult, details, wait, null, null, null, null);
  }

  private QueryResult(
      EnumQueryResult queryResult,
      String details, String wait,
      Integer pageId, String pageTitle, Integer pageOldRevId, Integer pageNewRevId) {
    this.queryResult = queryResult;
    this.details = details;
    this.wait = wait;
    this.pageId = pageId;
    this.pageTitle = pageTitle;
    this.pageOldRevId = pageOldRevId;
    this.pageNewRevId = pageNewRevId;
  }

  public boolean isQuerySuccessful() {
    if (queryResult != null) {
      return queryResult.isOk();
    }
    return false;
  }

  public String getDetails() {
    return details;
  }

  public String getWait() {
    return wait;
  }

  public Integer getPageId() {
    return pageId;
  }

  public String getPageTitle() {
    return pageTitle;
  }

  public Integer getPageOldRevId() {
    return pageOldRevId;
  }

  public Integer getPageNewRevId() {
    return pageNewRevId;
  }

  @Override
  public String toString() {
    return ((queryResult != null) ? queryResult.getText() : "") +
           ((details != null) ? " - " + details : "");
  }
}
