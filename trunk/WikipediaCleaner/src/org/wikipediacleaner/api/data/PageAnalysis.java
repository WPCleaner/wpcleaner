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

package org.wikipediacleaner.api.data;


/**
 * An analysis of a page.
 */
public class PageAnalysis {

  private final Page page;
  private final String contents;

  /**
   * @param page Page.
   * @param contents Page contents (may differ from page.getContents()).
   */
  public PageAnalysis(Page page, String contents) {
    this.page = page;
    this.contents = contents;
  }

  /**
   * @return Page.
   */
  public Page getPage() {
    return page;
  }

  /**
   * @return Page contents.
   */
  public String getContents() {
    return contents;
  }
}
