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

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a title (== Title ==). 
 */
public class PageElementTitle extends PageElement {

  private final int firstLevel;
  private final int secondLevel;
  private final String titleNotTrimmed;
  private final String title;
  private final String afterTitleNotTrimmed;

  /**
   * Analyze contents to check if it matches a title.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementTitle analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Check that this the beginning of a line
    if ((index > 0) && (contents.charAt(index - 1) != '\n')) {
      return null;
    }
    int beginIndex = index;

    // Check that the line is ending properly
    int endIndex = contents.indexOf('\n', index);
    if (endIndex < 0) {
      endIndex = contents.length();
    }

    // Compute first title level
    int firstLevel = 0;
    while ((index < endIndex) && (contents.charAt(index) == '=')) {
      index++;
      firstLevel++;
    }
    if (index >=  endIndex) {
      return null;
    }

    // Analyze possible text after title
    int tmpIndex = endIndex - 1;
    while ((tmpIndex > index) && (contents.charAt(tmpIndex) != '=')) {
      tmpIndex--;
    }
    if (tmpIndex <= index) {
      return null;
    }
    int afterTitleIndex = tmpIndex + 1;

    // Compute second title level
    int secondLevel = 0;
    while ((tmpIndex > index) && (contents.charAt(tmpIndex) == '=')) {
      tmpIndex--;
      secondLevel++;
    }
    if (tmpIndex < index) {
      return null;
    }

    return new PageElementTitle(
        beginIndex, endIndex,
        firstLevel, secondLevel,
        contents.substring(index, tmpIndex + 1),
        contents.substring(afterTitleIndex, endIndex));
  }

  /**
   * @return Title level.
   */
  public int getLevel() {
    return Math.min(firstLevel, secondLevel);
  }

  /**
   * @return True if there's nothing questionable about this title.
   */
  public boolean isCoherent() {
    return (firstLevel == secondLevel);
  }

  /**
   * @return Number of "=" before the title.
   */
  public int getFirstLevel() {
    return firstLevel;
  }

  /**
   * @return Number of "=" after the title.
   */
  public int getSecondLevel() {
    return secondLevel;
  }

  /**
   * @return Title itself.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @return Text after title.
   */
  public String getAfterTitle() {
    return afterTitleNotTrimmed;
  }

  private PageElementTitle(
      int beginIndex, int endIndex,
      int firstLevel, int secondLevel,
      String title, String afterTitle) {
    super(beginIndex, endIndex);
    this.firstLevel = firstLevel;
    this.secondLevel = secondLevel;
    this.titleNotTrimmed = title;
    this.title = (title != null) ? title.trim() : null;
    this.afterTitleNotTrimmed = afterTitle;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < firstLevel; i++) {
      sb.append('=');
    }
    sb.append(titleNotTrimmed);
    for (int i = 0; i < secondLevel; i++) {
      sb.append('=');
    }
    if (afterTitleNotTrimmed != null) {
      sb.append(afterTitleNotTrimmed);
    }
    return sb.toString();
  }

  /**
   * @param level Title level.
   * @param title Title text.
   * @return Textual representation of the title.
   */
  public static String createTitle(int level, String title) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < level; i++) {
      sb.append('=');
    }
    sb.append(' ');
    if (title != null) {
      sb.append(title.trim());
      sb.append(' ');
    }
    for (int i = 0; i < level; i++) {
      sb.append('=');
    }
    return sb.toString();
  }
}
