/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

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
   * @param comments Comments in the page.
   * @return Block details it there's a block.
   */
  public static PageElementTitle analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index,
      List<PageElementComment> comments) {
    // Verify arguments
    if (contents == null) {
      return null;
    }
    int maxLength = contents.length();

    // Check that this is an equal sign at the beginning of a line
    if ((index > 0) && (contents.charAt(index - 1) != '\n')) {
      return null;
    }
    int beginIndex = index;

    // Compute first title level
    int firstLevel = 0;
    while ((index < maxLength) && (contents.charAt(index) == '=')) {
      index++;
      firstLevel++;
    }
    if (index >=  maxLength) {
      return null;
    }
    int beginTitleIndex = index;

    // Analyze title
    boolean endFound = false;
    int secondLevel = 0;
    int lastEqualIndex = index;
    int endTitleIndex = index;
    while ((index < maxLength) && (contents.charAt(index) != '\n')) {
      char currentChar = contents.charAt(index);
      int nextIndex = index + 1;
      if (Character.isWhitespace(currentChar)) {
        // Nothing to do, continue
      } else if (currentChar == '=') {
        // Equal sign, possible end of title
        if (!endFound) {
          endTitleIndex = index;
          endFound = true;
          secondLevel = 0;
        }
        secondLevel++;
        lastEqualIndex = index;
      } else if (currentChar == '<') {
        PageElementComment comment = null;
        for (PageElementComment tmpComment : comments) {
          if (tmpComment.getBeginIndex() == index) {
            comment = tmpComment;
          }
        }
        if (comment == null) {
          endFound = false;
        } else {
          nextIndex = comment.getEndIndex();
        }
      } else {
        endFound = false;
      }
      index = nextIndex;
    }
    int endIndex = index;

    if (!endFound) {
      return null;
    }

    return new PageElementTitle(
        beginIndex, endIndex,
        firstLevel, secondLevel,
        contents.substring(beginTitleIndex, endTitleIndex),
        contents.substring(lastEqualIndex + 1, endIndex));
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
  public static String createTitle(int level, String title, String after) {
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
    if ((after != null) && (after.trim().length() > 0)) {
      sb.append(' ');
      sb.append(after.trim());
    }
    return sb.toString();
  }
}
