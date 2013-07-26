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


/**
 * A class to save comments and information for a Page.
 */
public class PageComment {

  private String comment;
  private Integer maxArticles;
  private Integer maxMainArticles;
  private Integer maxOtherArticles;
  private Integer maxTemplateArticles;

  /**
   * Create a bean for storing comments and information for a page.
   */
  public PageComment() {
    //
  }

  /**
   * @return Comments on the page.
   */
  public String getComment() {
    return comment;
  }

  /**
   * @param comment Comments on the page.
   */
  public void setComment(String comment) {
    this.comment = comment;
  }

  /**
   * @return Maximum number of back links from the main name space.
   */
  public Integer getMaxMainArticles() {
    return maxMainArticles;
  }

  /**
   * @param max Maximum number of back links from the main name space.
   */
  public void setMaxMainArticles(Integer max) {
    this.maxMainArticles = max;
  }

  /**
   * @return Maximum number of back links from the template name space.
   */
  public Integer getMaxTemplateArticles() {
    return maxTemplateArticles;
  }

  /**
   * @param max Maximum number of back links from the template name space.
   */
  public void setMaxTemplateArticles(Integer max) {
    this.maxTemplateArticles = max;
  }

  /**
   * @return Maximum number of back links from other name spaces.
   */
  public Integer getMaxOtherArticles() {
    return maxOtherArticles;
  }

  /**
   * @param max Maximum number of back links from other name spaces.s
   */
  public void setMaxOtherArticles(Integer max) {
    this.maxOtherArticles = max;
  }

  /**
   * @return Maximum number of back links.
   */
  public Integer computeMaxArticles() {
    if ((maxMainArticles == null) &&
        (maxTemplateArticles == null) &&
        (maxOtherArticles == null)) {
      return null;
    }
    return Integer.valueOf(
        (maxMainArticles != null ? maxMainArticles.intValue() : 0) +
        (maxTemplateArticles != null ? maxTemplateArticles.intValue() : 0) +
        (maxOtherArticles != null ? maxOtherArticles.intValue() : 0));
  }

  /**
   * @return Maximum number of back links.
   */
  public Integer retrieveOldMaxArticles() {
    return maxArticles;
  }

  /**
   * @param max Maximum number of back links.
   * @deprecated Maximum number of back links is now computed from other maximums.
   */
  @Deprecated
  public void setMaxArticles(Integer max) {
    this.maxArticles = max;
  }

  /**
   * Compute correct values.
   */
  public void fixValues() {
    if ((maxArticles != null) && (maxOtherArticles == null)) {
      int tmp = maxArticles.intValue();
      if (maxMainArticles != null) {
        tmp -= maxMainArticles.intValue();
      }
      if (maxTemplateArticles != null) {
        tmp -= maxTemplateArticles.intValue();
      }
      maxOtherArticles = Integer.valueOf(tmp);
    }
  }
}
