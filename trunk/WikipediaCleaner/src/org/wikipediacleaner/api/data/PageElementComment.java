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
 * Class containing information about a comment (<!-- Comment -->). 
 */
public class PageElementComment {
  private final int beginIndex;
  private final int endIndex;
  private final String commentNotTrimmed;
  private final String comment;

  /**
   * Analyze contents to check if it matches a comment.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementComment analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }
    if ((index < 0) || (index >= contents.length())) {
      return null;
    }

    // Check that it starts as a comment
    if (!contents.startsWith("<!--", index)) {
      return null;
    }
    int beginComment = index + 4;

    // Check that the comment ends
    int endIndex = contents.indexOf("-->", beginComment);
    if (endIndex < 0) {
      return null;
    }

    return new PageElementComment(
        index, endIndex + 3,
        contents.substring(beginComment, endIndex));
  }

  public int getBeginIndex() {
    return beginIndex;
  }

  public int getEndIndex() {
    return endIndex;
  }

  public String getComment() {
    return comment;
  }

  private PageElementComment(
      int beginIndex, int endIndex,
      String comment) {
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
    this.commentNotTrimmed = comment;
    this.comment = (comment != null) ? comment.trim() : null;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("<!--");
    sb.append(commentNotTrimmed);
    sb.append("-->");
    return sb.toString();
  }
}
