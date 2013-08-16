/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.util.List;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.Page;


/**
 * Information about an error type for a page.
 */
public class CheckErrorPage {

  private final CheckErrorAlgorithm algorithm;
  private final Page page;
  private final boolean inWhiteList;
  private boolean errorFound;
  private List<CheckErrorResult> results;

  /**
   * @param page Page.
   * @param algorithm Algorithm.
   */
  public CheckErrorPage(Page page, CheckErrorAlgorithm algorithm) {
    this.page = page;
    this.algorithm = algorithm;
    this.inWhiteList = ((page != null) && (algorithm != null)) ?
        algorithm.isInWhiteList(page.getTitle()) : false;
    this.errorFound = false;
    this.results = null;
  }

  /**
   * @return Page.
   */
  public Page getPage() {
    return page;
  }

  /**
   * @return Algorithm.
   */
  public CheckErrorAlgorithm getAlgorithm() {
    return algorithm;
  }

  /**
   * @return Page in white list ?
   */
  public boolean isInWhiteList() {
    return inWhiteList;
  }

  /**
   * @param errorFound Flag indicating if errors have been found.
   * @param results Results.
   */
  public void setResults(
      boolean errorFound,
      List<CheckErrorResult> results) {
    this.errorFound = errorFound;
    this.results = results;
  }

  /**
   * @return Flag indicating if errors have been found.
   */
  public boolean getErrorFound() {
    return errorFound;
  }

  /**
   * @return Results.
   */
  public List<CheckErrorResult> getResults() {
    return results;
  }

  /**
   * @return Results count.
   */
  public int getResultsCount() {
    if (results == null) {
      return 0;
    }
    return results.size();
  }

  /**
   * @return Active results count.
   */
  public int getActiveResultsCount() {
    if (results == null) {
      return 0;
    }
    int count = 0;
    for (CheckErrorResult result : results) {
      if ((result.getErrorLevel() == CheckErrorResult.ErrorLevel.ERROR) ||
          (result.getErrorLevel() == CheckErrorResult.ErrorLevel.WARNING)) {
        count++;
      }
    }
    return count;
  }

  /**
   * @return String representation.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return page.toString();
  }
}
