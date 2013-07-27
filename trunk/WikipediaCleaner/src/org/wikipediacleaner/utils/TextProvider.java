/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.util.Collection;


/**
 * A basic interface for providing text.
 */
public interface TextProvider {

  /**
   * @return Possible texts.
   */
  public Collection<String> getTexts();
}
