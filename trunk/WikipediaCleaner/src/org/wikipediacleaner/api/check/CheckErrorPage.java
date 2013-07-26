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
}
