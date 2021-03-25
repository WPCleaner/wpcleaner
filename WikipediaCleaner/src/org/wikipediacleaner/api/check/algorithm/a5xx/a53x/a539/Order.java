/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a539;

/**
 * Enumeration for possible order changes.
 */
enum Order {
  MUST_KEEP,
  BOTH_POSSIBLE,
  MUST_INVERT;

  /**
   * @return True if tags order can be keep.
   */
  boolean canKeepOrder() {
    if ((this == MUST_KEEP) || (this == BOTH_POSSIBLE)) {
      return true;
    }
    return false;
  }

  /**
   * @return True if tags order can be inverted.
   */
  boolean canInvertOrder() {
    if ((this == MUST_INVERT) || (this == BOTH_POSSIBLE)) {
      return true;
    }
    return false;
  }
}