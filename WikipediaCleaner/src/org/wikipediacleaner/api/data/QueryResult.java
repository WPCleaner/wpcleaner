/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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

  public EnumQueryResult getQueryResult() {
    return queryResult;
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
