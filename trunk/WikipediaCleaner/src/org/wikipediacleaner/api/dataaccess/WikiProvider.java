/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dataaccess;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Interface for providing access to a wiki.
 */
public interface WikiProvider {

  /**
   * @return Wiki.
   */
  public EnumWikipedia getWiki();
}
