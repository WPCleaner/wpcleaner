/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a539;

/**
 * Enumeration for possible order with formatting elements.
 */
enum OrderFormatting {
  FORMATTING_INSIDE,
  FORMATTING_OUTSIDE,
  FORMATTING_ANYWHERE;

  /**
   * @return True if formatting can be inside.
   */
  boolean canBeInside() {
    if ((this == FORMATTING_INSIDE) || (this == FORMATTING_ANYWHERE)) {
      return true;
    }
    return false;
  }

  /**
   * @return True if formatting can be outside.
   */
  boolean canBeOutside() {
    if ((this == FORMATTING_OUTSIDE) || (this == FORMATTING_ANYWHERE)) {
      return true;
    }
    return false;
  }
}