/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;


/**
 * An interface for versioned object.
 */
public interface Versioned {

  /**
   * @return Object version.
   */
  Integer getVersion();
}
