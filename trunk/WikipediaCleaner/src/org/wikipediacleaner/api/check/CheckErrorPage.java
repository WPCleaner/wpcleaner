/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

import java.util.ArrayList;

import org.wikipediacleaner.api.data.Page;


/**
 * Informations about an error type for a page.
 */
public class CheckErrorPage {

  private final CheckErrorAlgorithm algorithm;
  private final Page page;
  private ArrayList<CheckErrorResult> results;

  /**
   * @param page Page.
   * @param algorithm Algorithm.
   */
  public CheckErrorPage(Page page, CheckErrorAlgorithm algorithm) {
    this.page = page;
    this.algorithm = algorithm;
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
   * @param results Results.
   */
  public void setResults(ArrayList<CheckErrorResult> results) {
    this.results = results;
  }

  /**
   * @return Results.
   */
  public ArrayList<CheckErrorResult> getResults() {
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
}
