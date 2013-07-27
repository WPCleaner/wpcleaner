/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;


/**
 * An interface for versionned object.
 */
public interface Versionned {

  /**
   * @return Object version.
   */
  public Integer getVersion();
}
