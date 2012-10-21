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

package org.wikipediacleaner.api.data;


/**
   * Bean for holding internal links count.
 */
public class InternalLinkCount {

  /**
   * Link name.
   */
  private final String link;

  /**
   * Count of internal links.
   */
  private int internalLinkCount;

  /**
   * Count of good links through templates.
   */
  private int goodTemplateCount;

  /**
   * Count of help needed links through templates.
   */
  private int helpNeededTemplateCount;

  /**
   * Count of incorrect links through templates.
   */
  private int incorrectTemplateCount;

  /**
   * @param link Link name.
   */
  InternalLinkCount(String link) {
    this.link = link;
  }

  /**
   * @return Link name.
   */
  public String getLink() {
    return link;
  }

  /**
   * @return Total count of links.
   */
  public int getTotalLinkCount() {
    return internalLinkCount +
        goodTemplateCount +
        helpNeededTemplateCount +
        incorrectTemplateCount;
  }

  /**
   * Increase the count of internal links.
   */
  public void addInternalLink() {
    internalLinkCount++;
  }

  /**
   * @return Count of internal links.
   */
  public int getInternalLinkCount() {
    return internalLinkCount;
  }

  /**
   * Increase the count of good links through templates.
   */
  public void addGoodTemplateLink() {
    goodTemplateCount++;
  }

  /**
   * @return Count of good links through templates.
   */
  public int getGoodTemplateCount() {
    return goodTemplateCount;
  }

  /**
   * Increase the count of help needed links through templates.
   */
  public void addHelpNeededTemplateLink() {
    helpNeededTemplateCount++;
  }

  /**
   * @return Count of help needed links through templates.
   */
  public int getHelpNeededTemplateCount() {
    return helpNeededTemplateCount;
  }

  /**
   * Increase the count of incorrect links through templates.
   */
  public void addIncorrectTemplateLink() {
    incorrectTemplateCount++;
  }

  /**
   * @return Count of incorrect links through templates.
   */
  public int getIncorrectTemplateCount() {
    return incorrectTemplateCount;
  }
}
