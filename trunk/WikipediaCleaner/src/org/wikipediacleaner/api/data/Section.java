/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * Bean for holding information a section.
 */
public class Section {

  /**
   * Title level in the table of contents.
   */
  private final int tocLevel;

  /**
   * Title level.
   */
  private final int level;

  /**
   * Title.
   */
  private final String line;

  /**
   * Title number.
   */
  private final String number;

  /**
   * Title index.
   */
  private final int index;

  public Section(
      int tocLevel, int level,
      String line, String number, int index) {
    this.tocLevel = tocLevel;
    this.level = level;
    this.line = line;
    this.number = number;
    this.index = index;
  }

  /**
   * @return Title level in the table of contents.
   */
  public int getTocLevel() {
    return tocLevel;
  }

  /**
   * @return Title level.
   */
  public int getLevel() {
    return level;
  }

  /**
   * @return Title.
   */
  public String getLine() {
    return line;
  }

  /**
   * @return Title number.
   */
  public String getNumber() {
    return number;
  }

  /**
   * @return Title index.
   */
  public int getIndex() {
    return index;
  }
}
