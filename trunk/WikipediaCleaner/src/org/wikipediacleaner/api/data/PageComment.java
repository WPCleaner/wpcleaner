/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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
 * A class to save comments for a Page.
 */
public class PageComment {

  private String comment;
  private Integer maxArticles;
  private Integer maxMainArticles;
  private Integer maxTemplateArticles;

  public PageComment() {
    //
  }

  public String getComment() {
    return comment;
  }
  public void setComment(String comment) {
    this.comment = comment;
  }

  public Integer getMaxArticles() {
    return maxArticles;
  }
  public void setMaxArticles(Integer max) {
    this.maxArticles = max;
  }

  public Integer getMaxMainArticles() {
    return maxMainArticles;
  }
  public void setMaxMainArticles(Integer max) {
    this.maxMainArticles = max;
  }

  public Integer getMaxTemplateArticles() {
    return maxTemplateArticles;
  }
  public void setMaxTemplateArticles(Integer max) {
    this.maxTemplateArticles = max;
  }
}
