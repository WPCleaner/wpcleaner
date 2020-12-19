/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a501;

/**
 * Utility class to memorize possible replacements.
 */
class Replacement implements Comparable<Replacement> {
  private final int begin;
  private final int end;
  private final String comment;
  private final boolean otherPattern;
  private final String replacement;
  private final boolean automatic;
  private Boolean multiple;

  public Replacement(
      int begin, int end,
      String comment, boolean otherPattern,
      String replacement, boolean automatic) {
    this.begin = begin;
    this.end = end;
    this.otherPattern = otherPattern;
    this.comment = comment;
    this.replacement = replacement;
    this.automatic = automatic;
    this.multiple = null;
  }

  public int getBegin() {
    return begin;
  }

  public int getEnd() {
    return end;
  }

  public String getComment() {
    return comment;
  }

  public boolean isOtherPattern() {
    return otherPattern;
  }

  public String getReplacement() {
    return replacement;
  }

  public boolean isAutomatic() {
    return automatic;
  }

  public Boolean isMultiple() {
    return multiple;
  }

  public void setMultiple() {
    multiple = Boolean.TRUE;
  }
  /**
   * @param o
   * @return
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(Replacement o) {
    if (begin != o.begin) {
      return (begin < o.begin ? -1 : 1);
    }
    if (end != o.end) {
      return (end < o.end ? -1 : 1);
    }
    return 0;
  }
}
